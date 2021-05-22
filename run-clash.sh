#!/bin/sh

cd "$(dirname $0)" || exit $?

rm -rf vhdl
rm -f ../clash-compiler/tests/shouldwork/Cores/Floating/samplerom.bin
cabal run clash -- -Wall -fclash-float-support --vhdl -i../clash-compiler/tests/shouldwork/Cores/Floating Xilinx
