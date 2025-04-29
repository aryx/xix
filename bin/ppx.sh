#!/bin/bash
PPX=`ocamlfind query ppx_deriving`
exec $PPX/ppx_deriving package:ppx_deriving.show "$@"
