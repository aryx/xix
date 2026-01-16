(*s: objects/Object_file.ml *)
(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Regexp.Operators
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(* TODO? could also add file origin? *)
(*s: type [[Object_file.t]] *)
(* An object file (.o) in Plan 9 is really just a serialized assembly AST *)
type 'instr t = {
  prog:  'instr Ast_asm.program;
  arch: Arch.t
}
(*e: type [[Object_file.t]] *)

(* less: could be sha1 of ast_asmxxx.ml for even safer marshalling *)
(*s: constant [[Object_file.version]] *)
let version = 8
(*e: constant [[Object_file.version]] *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)
(* TODO? can normalize before? or check every invariants?
 * TODO? pass and save arch name string? like in goken? and
 * check for it at loading time?
 *)
(*s: function [[Object_file.save]] *)
let save (arch : Arch.t) (obj : 'instr Ast_asm.program) (chan : Chan.o) : unit =
  Logs.info (fun m -> m "Saving %s object in %s" (Arch.thestring arch)
      (Chan.destination chan));
  output_value chan.oc (version, { prog = obj; arch })
(*e: function [[Object_file.save]] *)

(* for slightly safer marshalling
 * TODO: exception WrongArch ?
 *)
(*s: exception [[Object_file.WrongVersion]] *)
exception WrongVersion
(*e: exception [[Object_file.WrongVersion]] *)

(*s: function [[Object_file.load]] *)
let load (chan : Chan.i) : 'instr t =
  Logs.info (fun m -> m "Loading object %s" (Chan.origin chan));
  let (ver, obj) = input_value chan.ic in
  if ver <> version
  then raise WrongVersion
  else obj
(*e: function [[Object_file.load]] *)

(*s: function [[Object_file.is_obj_filename]] *)
let is_obj_filename (file : Fpath.t) : bool =
  !!file =~ ".*\\.o[5v]$"
(*e: function [[Object_file.is_obj_filename]] *)
(*e: objects/Object_file.ml *)
