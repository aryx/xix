(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
(* for field access for ocaml-light *)
open Chan

(* todo: use absolute path? realpath? *)
type t = 
  Ast_asm5.program * Location_cpp.location_history list

(* less: could be sha1 of ast_asm5.ml for even safer marshalling *)
let version = 6

(* can normalize before? or check every invariants? *)
let save (obj : t) (chan : Chan.o) : unit =
    output_value chan.oc (version, obj)

(* for safer marshalling *)
exception WrongVersion

let load (chan : Chan.i) : t =
  let (ver, obj) = input_value chan.ic in
  if ver <> version
  then raise WrongVersion
  else obj
