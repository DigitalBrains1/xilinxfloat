#!/bin/sh

tail -n+0 -f "$(dirname $0)/build/vivado.log" | \
  grep -E '^Wall time|^Testbench|^Error|^WARNING'
