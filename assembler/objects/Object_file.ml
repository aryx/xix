(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
(* for field access for ocaml-light *)
open Chan

type t = Ast_asm5.program

(* less: could be sha1 of ast_asm5.ml for even safer marshalling *)
let version = 6

(* can normalize before? or check every invariants? *)
let save5 (obj : t) (chan : Chan.o) : unit =
  Logs.info (fun m -> m "Saving object in %s" (Chan.destination chan));
  output_value chan.oc (version, obj)

(* for safer marshalling *)
exception WrongVersion

let load5 (chan : Chan.i) : t =
  Logs.info (fun m -> m "Loading object %s" (Chan.origin chan));
  let (ver, obj) = input_value chan.ic in
  if ver <> version
  then raise WrongVersion
  else obj
