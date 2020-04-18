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

../_build/default/benchmark/mpst/pingpong/pingpong.exe +time cycles alloc gc percentage speedup samples -quota $QUOTA -sexp >mpst/pingpong/results/table_sexp_ev.txt
