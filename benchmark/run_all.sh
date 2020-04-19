#!/bin/bash
cd "$(dirname "$0")"

if [ $# != 1 ];
then
    echo 'OCaml-MPST benchmark script'
    echo 'Synopsis:'
    echo
    echo '  ./run_all.sh <seconds>'
    echo
    echo '  where <seconds> is the "quota" for each benchmark item'
    echo '  (./run_all.sh 0.1s is quick but not very reliable. default is 10s.)'
    echo
    exit 1
fi

set -e

QUOTA=$1

DIR=`mktemp -d -p results`
cd $DIR

echo Saving raw results to $DIR ..

set -v

opam switch ocaml-mpst-ev
eval `opam env`
dune clean

dune exec ../../ev/ev_pingpong.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA
dune exec ../../mpst/pingpong/pingpong.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA

opam switch ocaml-mpst-lwt
eval `opam env`
dune clean

dune exec ../../lwt/lwt_pingpong.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA
dune exec ../../mpst/pingpong/pingpong.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA
dune exec ../../mpst/nping/nping.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA
dune exec ../../mpst/chameleons/chameleons.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA

echo Saved raw results to $DIR .

dune exec ../../mpst/pingpong/pingpong.exe -- -load $(echo $(ls *s.txt) | sed 's/ / -load /g') -sexp >../table_sexp.txt
