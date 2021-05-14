package require cmdline
package require json

namespace eval clash {
    variable scriptDir [file dirname [info script]]
    variable scriptFile [info script]
    variable options
    variable db
    variable tbs

    namespace export buildDB addClashFiles runClashScripts selectTB \
            selectTBi buildProject runAllTBs formatTimeMilli

    proc parseCmdLine {} {
        variable options

        if [info exists options] {
            return
        }

        set opts {
            {clash-hdldir.arg "../vhdl" "Where Clash generated HDL files are"}
            {no-quit                    "Don't quit after running simulation"}
        }
        array set options [::cmdline::getoptions ::argv $opts]
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
            if [string is true $isTB] {
                puts "New test bench: $top"
            } else {
                puts "New top entity: $top"
            }
            dict set db $top isTB $isTB
            dict set db $top hdlFiles {}
            dict set db $top tclFiles {}
            foreach fileEntry [dict get $manifest files] {
                set name [dict get $fileEntry name]
                if [string match {*.vhdl} $name] {
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
            if [string is true $isTB] {
                lappend tbs $top
            }
            foreach dependency [dict get $manifest dependencies transitive] {
                parseManifest "[file dirname \
                        $manifestF]/../$dependency/clash-manifest.json" false
            }
        }

        set db [dict create]
        set tbs [list]
        foreach tbManifestF \
                [glob -type f -directory $options(clash-hdldir) \
                    *TB/clash-manifest.json] {
            parseManifest $tbManifestF true
        }
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
        dict for {top topDict} $db {
            foreach clashTclFile [dict get $topDict tclFiles] {
                source $clashTclFile
            }
        }
        update_compile_order -fileset sources_1
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
        variable db
        variable tbs

        set tstart [clock milliseconds]
        if [file exists xilinxfloat] {
            error "Refusing to overwrite existing file/dir 'xilinxfloat'"
        }
        source [file join $scriptDir xilinxfloat.tcl]

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

        foreach tb $tbs {
            set tstart [clock milliseconds]
            selectTB $tb
            launch_sim
            close_sim
            set delta [expr {([clock milliseconds] - $tstart) / 1000.0}]
            puts "Wall time spent running $tb: [format {%0.3f} $delta] sec"
        }
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
