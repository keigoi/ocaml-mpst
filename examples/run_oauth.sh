#!/bin/bash
cd "$(dirname "$0")"
set -e
set -v

opam switch ocaml-mpst-lwt
eval `opam env`
dune build @oauth/all

# Starting the OAuth server.
# An HTTPS-enabled web server is required.
# Add the following configuration to your HTTPS server:
#
# ProxyPass /scribble http://127.0.0.1:8080/scribble
# ProxyPassReverse /scribble http://127.0.0.1:8080/scribble
#
# and open: https://your-server-domain.com/scribble/oauth
#
# SSH port forwarding is useful for this purpose. For example,
#
#   ssh your-server-domain.com -R127.0.0.1:8080:127.0.0.1:8080
#
# will open 127.0.0.1:8080 on your server, and
# redirect connections to this host.
#
# After finishing experiments, do not forget to
# kill the OAuth server:
#
#   killall oauth.exe
#
../_build/default/examples/oauth/oauth.exe &
firefox https://keigoimai.info/scribble/oauth
