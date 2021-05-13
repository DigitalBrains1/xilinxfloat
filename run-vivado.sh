#!/bin/sh

mkdir -p "$(dirname $0)"/build
cd "$(dirname $0)"/build || exit $?

vivado -mode tcl -source ../vivado/simulate.tcl -tclargs "$@"
rm -rf xilinxfloat
