open Common

(* use commons/byte_order.ml? *)

let bp_long x =
  raise Todo

let bp_bool b =
  raise Todo

let bp_rect r =
  raise Todo

let bp_color c =
  raise Todo

(* alt:
 * marshall full AST if kernel was written in OCaml:
 * 
type msg =
  | AllocImage of int

  | Draw of int

  | Line of int
  | Ellipse of int
  | Arc of int
 *)

