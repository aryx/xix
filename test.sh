#!/bin/sh
# Basic testing.
# 
# This script assume you ran first ./bootstrap-mk.sh before.
#
# LATER: ideally later we could switch to rc instead of sh

# any error should abort the script
set -e
# for showing the executed commands (verbose)
set -x

# Just make sure at least the programs are not segfaulting
# or dynamic linking errors
./bin/mk --help
./bin/rc --help
./assembler/5a --help
./linker/5l --help
./compiler/5c --help
# TODO: add a --help, and exit 0 on --help, not 2
#./lex/ocamllex --help
#./yacc/ocamlyacc --help
#./version-control/ogit --help
