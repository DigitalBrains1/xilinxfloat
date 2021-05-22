source [file join [file dirname [info script]] clashFunctions.tcl]
namespace import clash::*
puts "Wall time start: [formatTimeMilli [clock milliseconds]]"
buildProject
runAllTBs
puts "Wall time finish: [formatTimeMilli [clock milliseconds]]"
if {!$clash::options(no-quit)} { quit }
