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

pushd mpst/pingpong/results
dune exec ../pingpong.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA -sexp >table_sexp_lwt.txt
popd; pushd mpst/nping/results
dune exec ../nping.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA -sexp >table_sexp_lwt.txt
popd; pushd mpst/chameleons/results
dune exec ../chameleons.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA -sexp >table_sexp_lwt.txt
popd; pushd mpst/chameleons/results_pipes
dune exec ../chameleons_pipes.exe -- +time cycles alloc gc percentage speedup samples -save -fork -quota $QUOTA -sexp >table_sexp_lwt.txt
