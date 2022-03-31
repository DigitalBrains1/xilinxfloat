#!/bin/sh

cd "$(dirname $0)" || exit $?
mkdir -p build

cabal run clash -- -Wall --vhdl -fclash-hdldir build/hdl Floating
