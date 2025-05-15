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

(* $Id: digest.ml,v 1.3 1997/04/11 13:57:48 xleroy Exp $ *)

(* Message digest (MD5) *)

type t = string

external unsafe_string: string -> int -> int -> t = "md5_string"
external channel: in_channel -> int -> t = "md5_chan"

let string str =
  unsafe_string str 0 (String.length str)

let substring str ofs len =
  if ofs < 0 or ofs + len > String.length str
  then invalid_arg "Digest.substring"
  else unsafe_string str ofs len

let file filename =
  let ic = open_in filename in
  let d = channel ic (in_channel_length ic) in
  close_in ic;
  d

let output chan digest =
  output chan digest 0 16

let input chan =
  let digest = String.create 16 in
  really_input chan digest 0 16;
  digest
