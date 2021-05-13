#!/bin/sh

cd "$(dirname $0)" || exit $?
mkdir -p build

rm -rf build/hdl
rm -f ../clash-compiler/tests/shouldwork/Cores/Floating/samplerom.bin
cabal run clash -- -Wall -fclash-float-support --vhdl -fclash-hdldir build/hdl -i../clash-compiler/tests/shouldwork/Cores/Floating Xilinx
