#!/bin/sh

# This is used via some 'ocamlc -pp remove_xix_open.sh' in bootstrap-mk.sh
# and a few mkfiles so we can compile xix with ocaml-light which does not know
# about (wrapped true) dune generated modules.

sed '/^open Xix_/d' "$1"
