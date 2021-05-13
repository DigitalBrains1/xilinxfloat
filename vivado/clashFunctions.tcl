package require cmdline
package require json

set clashTclScriptDir [file dirname [info script]]

proc reloadClashFunctions {} {
    uplevel #0 {source [file join $clashTclScriptDir clashFunctions.tcl]}
}

proc parseCmdLine {} {
    global cmdOpts

    set options {
        {clash-hdldir.arg "../vhdl" "Where Clash generated HDL files are"}
        {no-quit                    "Don't quit after running simulation"}
    }
    array set cmdOpts [::cmdline::getoptions ::argv $options]
}

parseCmdLine

proc popuDB {} {
    global clashDB allTBs cmdOpts
    proc parseManifest {manifestF isTB} {
        global clashDB allTBs
        set manC [open $manifestF r]
        set manifest [json::json2dict [read $manC]]
        close $manC
        set top [dict get $manifest top_component name]
        if [dict exists $clashDB $top] {
            return
        }
        if [string is true $isTB] {
            puts "New test bench: $top"
        } else {
            puts "New top entity: $top"
        }
        dict set clashDB $top isTB $isTB
        dict set clashDB $top hdlFiles {}
        dict set clashDB $top tclFiles {}
        foreach fileEntry [dict get $manifest files] {
            set name [dict get $fileEntry name]
            if [string match {*.vhdl} $name] {
                dict with clashDB $top {
                    lappend hdlFiles "[file dirname $manifestF]/$name"
                }
            }
            if [string match {*.tcl} $name] {
                dict with clashDB $top {
                    lappend tclFiles "[file dirname $manifestF]/$name"
                }
            }
        }
        if [string is true $isTB] {
            lappend allTBs $top
        }
        foreach dependency [dict get $manifest dependencies transitive] {
            parseManifest "[file dirname \
                    $manifestF]/../$dependency/clash-manifest.json" false
        }
    }

    set clashDB [dict create]
    set allTBs [list]
    puts [array get cmdOpts]
    foreach tbManifestF \
            [glob -type f -directory $cmdOpts(clash-hdldir) \
                *TB/clash-manifest.json] {
        parseManifest $tbManifestF true
    }
}

proc addClashFiles {} {
    global clashDB
    dict for {top topDict} $clashDB {
        set fileset [expr {[dict get $topDict isTB] ? {sim_1} : {sources_1}}]
        add_files -fileset $fileset -norecurse [dict get $topDict hdlFiles]
        set_property library $top [get_files [dict get $topDict hdlFiles]]
    }
    update_compile_order -fileset sim_1
}

proc runClashScripts {} {
    global clashDB
    dict for {top topDict} $clashDB {
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
    global allTBs
    set tb [lindex $allTBs $nr]
    selectTB $tb
}

proc buildProject {} {
    global clashDB allTBs clashTclScriptDir
    set tstart [clock milliseconds]
    if [file exists xilinxfloat] {
        error {Refusing to overwrite existing file/dir 'xilinxfloat'}
    }
    source [file join $clashTclScriptDir xilinxfloat.tcl]

    popuDB

    addClashFiles
    runClashScripts

    set_property SOURCE_SET sources_1 [get_filesets sim_1]
    set_property RUNTIME all [get_filesets sim_1]
    set delta [expr {([clock milliseconds] - $tstart) / 1000.0}]
    puts "Wall time spent building project: [format {%0.3f} $delta] sec"
}

proc runAllTBs {} {
    global allTBs
    foreach tb $allTBs {
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
            {%a %b %d %T}].$fraction [clock format $seconds -format {%Z %Y}]"
}
