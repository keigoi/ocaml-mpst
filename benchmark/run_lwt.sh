#!/bin/bash
cd "$(dirname "$0")"
if [ $# -eq 0 ]
then
    QUOTA=10s
else
	QUOTA=$1
fi
echo running benchmark with quota of $QUOTA seconds.
set -e
set -v

opam switch ocaml-mpst-lwt
eval `opam env`
dune build @lwt/all

../_build/default/benchmark/lwt/lwt_pingpong.exe +time cycles alloc gc percentage speedup samples -quota $QUOTA -sexp >lwt/results/table_sexp.txt
