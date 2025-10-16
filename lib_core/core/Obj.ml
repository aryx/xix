(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


(* Operations on internal representations of values *)

type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"

external is_block : t -> bool = "obj_is_block"
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "obj_tag"
external size : t -> int = "%obj_size"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external new_block : int -> int -> t = "obj_block"

(* coupling: byterun/mlvalues.h, asmcomp/cmmgen.ml, printexc.ml *)
(* TODO? lazy_tag *)
let no_scan_tag = 251
let closure_tag = 250
let infix_tag = 249
let object_tag = 248
let abstract_tag = 251
let string_tag = 252
let double_tag = 253
let double_array_tag = 254
let final_tag = 255
