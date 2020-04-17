#!/bin/bash
cd "$(dirname "$0")"
set -e
set -v

dune build @all

# Starting the DNS server. After entering root password,
# Type:
#   host -t a nagoya.my.domain 127.0.0.01
#   ==> The server will answer 1.2.3.4
#
#   host -t a www.google.com 127.0.0.1
#   ==> The server will forward the query
sudo ../../_build/default/examples/dns/dnsserver.exe