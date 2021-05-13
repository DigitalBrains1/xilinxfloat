#!/bin/sh

cd "$(dirname $0)"/build || exit $?

vivado -mode tcl -source ../vivado/simulate.tcl -tclargs "$@" -t '*TB' simul
rm -rf tempproj
