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


opam switch ocaml-mpst-ev
eval `opam env`
dune clean
dune build @ev/all @mpst/all

./run_ev.sh $1
./run_mpst_ev.sh $1

opam switch ocaml-mpst-lwt
eval `opam env`
dune clean
dune build @lwt/all @mpst/all

./run_lwt.sh $1
./run_mpst_lwt.sh $1

