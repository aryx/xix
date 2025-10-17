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

(* Module [Obj]: operations on internal representations of values *)

(* Not for the casual user. *)

type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"

val is_block : t -> bool
external tag : t -> int = "caml_obj_tag"
external size : t -> int = "%obj_size"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external new_block : int -> int -> t = "caml_obj_block"

(* from 3.0 *)
external is_int : t -> bool = "%obj_is_int"

(* from 2.02 *)
val no_scan_tag : int
val closure_tag : int
val infix_tag : int
val object_tag : int
val abstract_tag : int
val string_tag : int
val double_tag : int
val double_array_tag : int
val final_tag : int
