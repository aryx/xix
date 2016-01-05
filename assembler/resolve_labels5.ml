open Common

(* see how little things 5a actually do *)
let resolve p =
  let _pc = ref 0 in
  let _h = Hashtbl.create 101 in

  (* todo: check for duplicate! better here than via lexing tricks *)

  (* first pass *)

  (* second pass *)
  raise Todo
