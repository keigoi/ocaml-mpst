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

dune build @mpst/all

../_build/default/benchmark/mpst/pingpong/pingpong.exe +time cycles alloc gc percentage speedup samples -quota $QUOTA -sexp >mpst/pingpong/results/table_sexp.txt
../_build/default/benchmark/mpst/nping/nping.exe +time cycles alloc gc percentage speedup samples -quota $QUOTA -sexp >mpst/nping/results/table_sexp.txt
../_build/default/benchmark/mpst/chameleons/chameleons.exe +time cycles alloc gc percentage speedup samples -quota $QUOTA -sexp >mpst/chameleons/results/table_sexp.txt
