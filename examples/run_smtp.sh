#!/bin/bash
cd "$(dirname "$0")"
set -e

if [ $# != 1 ];
then
    echo 'Synopsis:'
    echo
    echo '  ./run_smtp.sh hostname:portnumber'
    echo
    echo 'Currently, this SMTP client assumes that you have an'
    echo 'authentication-less SMTP server'
    echo '(e.g. the one from your ISP or your organisation).'
    echo 
    echo 'If you do not have the one, SSH port forwarding'
    echo 'might help. For example,'
    echo 
    echo  '  ssh your-server-domain.com -L1234:192.168.0.100:25'
    echo 
    echo  'will open port 1234 of your local PC, and'
    echo  'redirect connections to the port 25 of SMTP server 192.168.0.100.'
    exit 1
fi


set -v

opam switch ocaml-mpst-lwt
eval `opam env`
dune exec smtp/smtpclient.exe -- $*




