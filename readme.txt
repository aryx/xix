Plan 9 software ported to OCaml.

export ROOT=`pwd`
export objtype=386
ocamlrun ./bootstrap/mk depend
ocamlrun ./bootstrap/mk
