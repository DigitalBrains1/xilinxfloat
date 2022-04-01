#!/bin/bash

run_clash() {
  local TOP

  if [[ $# -eq 0 || $1 == simul ]]; then
    TOP=Floating
  elif [[ $1 == synth ]]; then
    TOP=PhysTB
  else
    TOP="$1"
  fi

  mkdir -p build

  cabal run clash -- -Wall --vhdl -fclash-hdldir build/hdl "$TOP"
}

run_vivado() {
  cd "$(dirname $0)"/build || exit $?

  if [[ $# -eq 0 || $1 == simul ]]; then
    shift
    vivado -mode tcl -source ../vivado/simulate.tcl -tclargs "$@" -t '*TB' simul
  elif [[ $# -eq 1 && $1 == synth ]]; then
    vivado -mode tcl -source ../vivado/buildProject.tcl -tclargs synth PhysTB.physAddTB
  elif [[ $1 == build ]]; then
    vivado -mode tcl -source ../vivado/buildProject.tcl -tclargs "$@"
  else
    echo Invocation error. Run without arguments for help. >&2
    exit 1
  fi
  rm -rf tempproj
}

cd "$(dirname $0)" || exit $?

unset CMD

if [ $# -eq 0 ]; then
  cat >&2 <<EOF
## Quick start:

$0 clash
$0 vivado

## Invocation:

$0 clash
$0 clash simul
    Build the test benches for verification in simulation

$0 clash synth
    Build PhysTB

$0 clash <module name>
    Build <module name>

$0 vivado
    Simulate all entities named "*TB"

$0 vivado simul [<tclargs> ..]
    Simulate all entities named "*TB" and pass additional arguments

$0 vivado synth
    Build the project for the physical test bench to synthesize and run on a
    dev board.

    When the project is built, you could type the Tcl command "start_gui" to
    work with the project in the GUI.

$0 vivado build [tclargs] <project template> <entity ..>
    Build a project based on a template and a list of Clash entities.
EOF
  exit 1
fi

CMD="$1"
shift
run_"$CMD" "$@"
