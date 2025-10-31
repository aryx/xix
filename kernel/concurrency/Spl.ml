open Common
open Types
open Spl_

(* Set Priority Level (=~ enable/disable interrupts) *)

type prio = Spl_.prio

let high () =
  raise Todo

let low () =
  raise Todo

let set _prio =
  Logs.err (fun m -> m "TODO: spl.set")
  (* raise Todo *)

let is_low () =
  raise Todo

