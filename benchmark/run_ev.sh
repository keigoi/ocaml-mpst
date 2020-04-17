#!/bin/bash
cd "$(dirname "$0")"
if [ $# -eq 0 ]
then
    QUOTA=10s
else
	QUOTA=$1
fi
set -e
set -v

dune build @ev

../_build/default/benchmark/ev/ev_pingpong.exe +time cycles alloc gc percentage speedup samples -quota $QUOTA -sexp >ev/results/table_sexp.txt
