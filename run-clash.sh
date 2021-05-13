#!/bin/sh

cd "$(dirname $0)" || exit $?

rm -rf vhdl
cabal run clash -- -fclash-float-support --vhdl -i../clash-compiler/tests/shouldwork/Cores/Floating Xilinx
