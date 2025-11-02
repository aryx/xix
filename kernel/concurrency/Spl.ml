open Common
open Types
open Spl_

(* Set Priority Level (=~ enable/disable interrupts) *)

type prio = Spl_.prio

let high () : prio =
  raise Todo

let low () : prio =
  raise Todo

let set (_prio : prio) : unit =
  raise Todo

let is_low () : bool =
  raise Todo

