(*s: version_control/blob.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type Blob.t *)
type t = bytes
(*e: type Blob.t *)

(*s: type Blob.hash *)
type hash = Sha1.t
(*e: type Blob.hash *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(*s: function Blob.read *)
let read ch =
  IO.read_all ch
(*e: function Blob.read *)

(*s: function Blob.write *)
let write blob ch =
  IO.nwrite ch blob
(*e: function Blob.write *)

(*****************************************************************************)
(* Show *)
(*****************************************************************************)

(*s: function Blob.show *)
let show x =
  print_string x
(*e: function Blob.show *)
(*e: version_control/blob.ml *)
