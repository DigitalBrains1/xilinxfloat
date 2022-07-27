#!/bin/sh

tail -n+0 -F "$(dirname $0)/build/vivado.log" | \
  grep -E '^Wall time|^Testbench|^Error|^ERROR|^CRITICAL WARNING|^WARNING|^@'
