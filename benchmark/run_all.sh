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

DIR=`mktemp -d -p .`
cd $DIR

echo Saving raw results to $DIR ..

set -v

# extend file open limit
ulimit -n 65535

opam switch ocaml-mpst-ev
eval `opam env`
dune clean

mkdir ev_bare_pingpong; pushd ev_bare_pingpong
dune exec ../../ev/ev_pingpong.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA
dune exec ../../ev/ev_pingpong.exe -- -load $(echo $(ls *s.txt) | sed 's/ / -load /g') -sexp >../../results/ev_bare_pingpong.txt
popd

mkdir ev_pingpong; pushd ev_pingpong
dune exec ../../mpst/pingpong/pingpong.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA
dune exec ../../mpst/pingpong/pingpong.exe -- -load $(echo $(ls *s.txt) | sed 's/ / -load /g') -sexp >../../results/ev_pingpong.txt
popd


opam switch ocaml-mpst-lwt
eval `opam env`
dune clean

mkdir -p lwt_bare_pingpong; pushd lwt_bare_pingpong
dune exec ../../lwt/lwt_pingpong.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA
dune exec ../../lwt/lwt_pingpong.exe -- -load $(echo $(ls *s.txt) | sed 's/ / -load /g') -sexp >../../results/lwt_bare_pingpong.txt
popd

mkdir -p lwt_pingpong; pushd lwt_pingpong
dune exec ../../mpst/pingpong/pingpong.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA
dune exec ../../mpst/pingpong/pingpong.exe -- -load $(echo $(ls *s.txt) | sed 's/ / -load /g') -sexp >../../results/lwt_pingpong.txt
popd

mkdir -p lwt_nping; pushd lwt_nping
dune exec ../../mpst/nping/nping.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA
dune exec ../../mpst/nping/nping.exe -- -load $(echo $(ls *s.txt) | sed 's/ / -load /g') -sexp >../../results/lwt_nping.txt
popd

mkdir -p lwt_chameleons; pushd lwt_chameleons
dune exec ../../mpst/chameleons/chameleons.exe -- +time cycles alloc gc percentage speedup samples -save -quota $QUOTA
dune exec ../../mpst/chameleons/chameleons.exe -- -load $(echo $(ls *s.txt) | sed 's/ / -load /g') -sexp >../../results/lwt_chameleons.txt
popd

