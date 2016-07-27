#!/bin/sh

source ~/plan9/env.sh
./mk $*

# sed for emacs compile mode | sed -e 's/\.c:([0-9]+)/.c:\1:1/'

