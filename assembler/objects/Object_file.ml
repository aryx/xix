(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Regexp_.Operators
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* An object (.o) in Plan 9 is really just a serialized assembly AST
 * TODO? could also add file origin?
 *)
type 'instr t = {
  prog:  'instr Ast_asm.program;
  arch: Arch.t
}


(* less: could be sha1 of ast_asmxxx.ml for even safer marshalling *)
let version = 6

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* TODO? can normalize before? or check every invariants?
 * TODO? pass and save arch name string? like in goken? and
 * check for it at loading time?
 *)
let save (arch : Arch.t) (obj : 'instr Ast_asm.program) (chan : Chan.o) : unit =
  Logs.info (fun m -> m "Saving %s object in %s" (Arch.thestring arch)
      (Chan.destination chan));
  output_value chan.oc (version, { prog = obj; arch })

(* for slightly safer marshalling
 * TODO: exception WrongArch ?
 *)
exception WrongVersion

let load (chan : Chan.i) : 'instr t =
  Logs.info (fun m -> m "Loading object %s" (Chan.origin chan));
  let (ver, obj) = input_value chan.ic in
  if ver <> version
  then raise WrongVersion
  else obj

let is_obj_filename (file : Fpath.t) : bool =
  !!file =~ ".*\\.o[5v]$"
