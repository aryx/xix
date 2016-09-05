
type opcode =
  | F of (unit -> unit)
  | S of string
  (* | I of int *)

type codevec = opcode array
