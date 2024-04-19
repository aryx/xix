#!/bin/sh
# Basic testing
# LATER: ideally later we could switch to rc instead of sh

# any error should abort the script
set -e
# for showing the executed commands (verbose)
set -x

# Just make sure at least the programs are not segfaulting
# or dynamic linking errors

./BOOTSTRAP/mk --help
./BOOTSTRAP/rc --help
