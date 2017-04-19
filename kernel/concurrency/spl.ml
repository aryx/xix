open Common

(* Set Priority Level (=~ enable/disable interrupts) *)


(* less: in plan9 there is really just high or low; no intermediate *)
type prio = Low | High

let high () =
  raise Todo

let low () =
  raise Todo

let set prio =
  raise Todo

let is_low () =
  raise Todo

