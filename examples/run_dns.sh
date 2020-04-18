#!/bin/bash
cd "$(dirname "$0")"
set -v

opam switch ocaml-mpst-lwt
eval `opam env`
dune build @dns/all

# Starting the DNS server. After entering root password,
# Type:
#   host -t a nagoya.my.domain 127.0.0.1
#   ==> The server will answer 1.2.3.4
#
#   host -t a www.google.com 127.0.0.1
#   ==> The server will forward the query
sudo ../_build/default/examples/dns/dnsserver.exe
