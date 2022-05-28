package require cmdline
package require json

# TODO: VHDL wants separate libraries for all entities, but Verilog does not
# work with that.
# TODO: SystemVerilog says:
#       WARNING: [XSIM 43-4100] "/home/peter/src/clash/xilinxfloat/build/hdl/Xilinx.addFloatEnableTB/addFloatEnableTB.sv" Line 5. Module addFloatEnableTB has a timescale but at least one module in design doesn't have timescale.

namespace eval clash {
    variable scriptDir [file dirname [info script]]
    variable scriptFile [info script]
    variable options
    variable db
    variable tbs

    namespace export buildDB addClashFiles runClashScripts listTBs selectTB \
            selectTBi buildProject runAllTBs formatTimeMilli

    proc parseCmdLine {} {
        variable options

        array unset options
        array set options {help 0 clash-hdldir ./hdl no-quit 0 t {}}
        set largv $::argv
        set opts {? h help clash-hdldir.arg no-quit t.arg}
        while {[set err [::cmdline::getopt largv $opts opt arg]]} {
            if {$err < 0} {
                puts $arg
                puts {}
                set options(help) 2
                break
            }
            switch $opt {
                t {
                    # This accumulates multiple values
                    lappend options($opt) $arg
                }
                ? -
                h -
                help {
                    # These are all synonyms
                    set options(help) 1
                }
                default {
                    set options($opt) $arg
                }
            }
        }
        if {[llength $largv] < 1} {
            puts "Please specify <project>"
            puts {}
            set options(help) 2
        }
        if {$options(help)} {
            puts "Usage:"
            puts {}
            puts -nonewline {-tclargs [-clash-hdldir <dir>] [-no-quit] }
            puts {[-t <testbench>]...}
            puts {         <project> [<entity>]...}
            puts "-tclargs -help"
            puts {}
            puts "Arguments:"
            puts "  <project>   Name of TCL script to build project with"
            puts -nonewline "  <entity>    Include the Clash logical entity "
            puts "<entity> and its dependencies."
            puts -nonewline "              The name refers to the name of a "
            puts "directory in -clash-hdldir. The"
            puts -nonewline "              name can be a shell glob to match "
            puts "multiple directory names."
            puts {}
            puts "Options:"
            puts -nonewline "  -clash-hdldir     Where Clash-generated HDL "
            puts "files are (default: ./hdl)"
            puts "  -no-quit          Don't quit after running simulation"
            puts -nonewline "  -t <testbench>    Same as <entity>, but "
            puts "includes it as a simulation source."
            puts -nonewline "  -help             Print this help text and exit "
            puts " (-? and -h are synonyms)"

            if {$options(help) == 2} {
                exit 1
            } else {
                exit 0
            }
        }
        set options(project) [lindex $largv 0]
        set options(ents) [lrange $largv 1 end]
        return
    }

    proc buildDB {} {
        variable options
        variable db
        variable tbs

        proc parseManifest {manifestF isTB} {
            variable db
            variable tbs

            set manC [open $manifestF r]
            set manifest [json::json2dict [read $manC]]
            close $manC
            set top [dict get $manifest top_component name]
            if [dict exists $db $top] {
                return
            }
            if {$isTB} {
                puts "New test bench: $top"
            } else {
                puts "New top entity: $top"
            }
            dict set db $top isTB $isTB
            dict set db $top hdlFiles {}
            dict set db $top tclFiles {}
            foreach fileEntry [dict get $manifest files] {
                set name [dict get $fileEntry name]
                if {
                       [string match {*.vhdl} $name]
                    || [string match {*.v} $name]
                    || [string match {*.sv} $name]
                } then {
                    dict with db $top {
                        lappend hdlFiles "[file dirname $manifestF]/$name"
                    }
                }
                if [string match {*.tcl} $name] {
                    dict with db $top {
                        lappend tclFiles "[file dirname $manifestF]/$name"
                    }
                }
            }
            if {$isTB} {
                lappend tbs $top
            }
            foreach dependency [dict get $manifest dependencies transitive] {
                parseManifest "[file dirname \
                        $manifestF]/../$dependency/clash-manifest.json" false
            }
        }

        set db [dict create]
        set tbs [list]
        if {[namespace exists tclIface]} {
            namespace delete tclIface
        }

        foreach ent $options(ents) {
            foreach manifestF \
                    [glob -type f -directory $options(clash-hdldir) \
                        $ent/clash-manifest.json] {
                parseManifest $manifestF false
            }
        }
        foreach tb $options(t) {
            foreach manifestF \
                    [glob -type f -directory $options(clash-hdldir) \
                        $tb/clash-manifest.json] {
                parseManifest $manifestF true
            }
        }

        dict for {top topDict} $db {
            foreach clashTclFile [dict get $topDict tclFiles] {
                loadTclIface $top $clashTclFile
            }
        }
    }

    # Populate a namespace with a Clash-generated Tcl interface.
    # Namespace is clash::tclIface::$top::$baseName
    proc loadTclIface {top clashTclFile} {
        # Evaluate script code inside temporary throwaway namespace to
        # separate its definitions from ours and reduce the chance of
        # accidentally corrupting our code.
        namespace eval tmp "source $clashTclFile"
        set baseName [file rootname [file tail $clashTclFile]]
        set ns [namespace current]::tclIface::${top}::${baseName}
        if {[namespace exists $ns]} {
            # Surely if this namespace already exists, it's just us
            # re-evaluating a previously evaluated script and we can just
            # drop the old contents.
            namespace delete $ns
        }
        tmp::createNamespace $ns
        namespace delete tmp
    }

    proc addClashFiles {} {
        variable db

        dict for {top topDict} $db {
            set fileset [expr {[dict get $topDict isTB] ? {sim_1}
                                                        : {sources_1}}]
            add_files -fileset $fileset -norecurse \
                    [dict get $topDict hdlFiles]
            set_property library $top [get_files [dict get $topDict hdlFiles]]
        }
        update_compile_order -fileset sources_1
        update_compile_order -fileset sim_1
    }

    proc runClashScripts {} {
        variable db

        if {![namespace exists tclIface]} {
            # There are no scripts
            return
        }

        # Identical names means identical IP, only one run needed even if it
        # occurs in multiple HDL directories.
        set seen [list]
        foreach topNs [namespace children tclIface] {
            foreach tclIface [namespace children $topNs] {
                set api [subst $${tclIface}::api]
                if {$api ne {0.1alpha1}} {
                    puts "Error: $tclIface doesn't implement an API we\
                        support: api = \"$api\"."
                    continue
                }
                set purpose [subst $${tclIface}::scriptPurpose]
                if {$purpose ne {createIp}} {
                    puts "Error: $tclIface::scriptPurpose bogus value\
                        \"$purpose\"."
                    continue
                }
                set ipName [subst $${tclIface}::ipName]
                if {$ipName in $seen} {
                    continue
                }
                ${tclIface}::createIp $ipName
                lappend seen $ipName
            }
        }
        update_compile_order -fileset sources_1
    }

    proc listTBs {} {
        variable tbs
        set i 0
        foreach tb $tbs {
            puts "[format %3d $i]: $tb"
            incr i
        }
    }

    proc selectTB top {
        set_property TOP $top [get_filesets sim_1]
        puts "Testbench selected: $top"
    }

    proc selectTBi nr {
        variable tbs
        set tb [lindex $tbs $nr]
        selectTB $tb
    }

    proc buildProject {} {
        variable scriptDir
        variable options
        variable db
        variable tbs

        set tstart [clock milliseconds]
        if {[file exists tempproj]} {
            error "Refusing to overwrite existing file/dir 'tempproj'"
        }
        set projects_dir [file join $scriptDir projects]
        set argv_save $::argv
        set ::argv {}
        set ::user_project_name tempproj
        set ::origin_dir_loc $projects_dir
        source [file join $projects_dir $options(project).tcl]
        set ::argv $argv_save
        unset ::user_project_name ::origin_dir_loc

        buildDB

        addClashFiles
        runClashScripts

        set_property SOURCE_SET sources_1 [get_filesets sim_1]
        set_property RUNTIME all [get_filesets sim_1]
        set delta [expr {([clock milliseconds] - $tstart) / 1000.0}]
        puts "Wall time spent building project: [format {%0.3f} $delta] sec"
    }

    proc runAllTBs {} {
        variable tbs

        set tstart_all [clock milliseconds]
        foreach tb $tbs {
            set tstart [clock milliseconds]
            selectTB $tb
            launch_sim
            close_sim
            set delta [expr {([clock milliseconds] - $tstart) / 1000.0}]
            puts "Wall time spent running $tb: [format {%0.3f} $delta] sec"
        }
        set delta [expr {([clock milliseconds] - $tstart_all) / 1000.0}]
        puts "Wall time spent running all testbenches: [format \
                {%0.3f} $delta] sec"
    }

    proc formatTimeMilli time {
        set seconds [expr {$time / 1000}]
        set fraction [format {%03u} [expr {$time % 1000}]]
        return "[clock format $seconds -format \
                {%a %b %d %T}].$fraction [clock format $seconds \
                -format {%Z %Y}]"
    }
}

clash::parseCmdLine
