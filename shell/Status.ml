(*s: Status.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

module R = Runtime

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small helpers to manipulate the "status" special variable (see Var.ml) *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(*s: function [[Status.setstatus]] *)
let setstatus s =
  Var.setvar "status" [s]
(*e: function [[Status.setstatus]] *)
(*s: function [[Status.getstatus]] *)
let getstatus () =
  let v = (Var.vlook "status").R.v in
  match v with
  | None -> ""
  | Some [x] -> x
  (* stricter: should never happen *)
  | Some _ -> failwith "getstatus: $status is a list with more than one element"
(*e: function [[Status.getstatus]] *)
(*s: function [[Status.concstatus]] *)
let concstatus s1 s2 =
  s1 ^ "|" ^ s2
(*e: function [[Status.concstatus]] *)
(*s: function [[Status.truestatus]] *)
let truestatus () : bool =
  let s = getstatus () in
  s = ""
  || s =~ "0+\\(|0+\\)*"
(*e: function [[Status.truestatus]] *)
(*e: Status.ml *)
