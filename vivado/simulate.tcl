source [file join [file dirname [info script]] clashFunctions.tcl]
puts "Wall time start: [formatTimeMilli [clock milliseconds]]"
buildProject
runAllTBs
puts "Wall time finish: [formatTimeMilli [clock milliseconds]]"
if [expr { !$cmdOpts(no-quit) }] { quit }
