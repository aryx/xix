(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)

(* for field access for ocaml-light *)
open Chan

(* less: could be sha1 of ast_asmxxx.ml for even safer marshalling *)
let version = 6

(* can normalize before? or check every invariants? *)
let save (obj : 'instr Ast_asm.program) (chan : Chan.o) : unit =
  Logs.info (fun m -> m "Saving object in %s" (Chan.destination chan));
  output_value chan.oc (version, obj)

(* for safer marshalling *)
exception WrongVersion

let load (chan : Chan.i) : 'instr Ast_asm.program =
  Logs.info (fun m -> m "Loading object %s" (Chan.origin chan));
  let (ver, obj) = input_value chan.ic in
  if ver <> version
  then raise WrongVersion
  else obj
