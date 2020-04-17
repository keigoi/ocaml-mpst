#!/bin/bash
cd "$(dirname "$0")"

./run_ev.sh $1
./run_mpst_ev.sh $1
./run_lwt.sh $1
./run_mpst_lwt.sh $1

