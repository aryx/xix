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

let setstatus s =
  Var.setvar "status" [s]

let getstatus () =
  let v = (Var.vlook "status").R.v in
  match v with
  | None -> ""
  | Some [x] -> x
  (* stricter: should never happen *)
  | Some _ -> failwith "getstatus: $status is a list with more than one element"

let concstatus s1 s2 =
  s1 ^ "|" ^ s2

let truestatus () =
  let s = getstatus () in
  s = "" || s =~ "0+\\(|0+\\)*"
