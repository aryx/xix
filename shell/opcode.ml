
type opcode =
  | F of ((unit -> unit) * string)
  | I of int
  | S of string

type codevec = opcode array
