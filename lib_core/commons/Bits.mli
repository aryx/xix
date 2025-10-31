
type t = (int * int) list
[@@deriving show]

type int32 = t
type int64 = t

val int_of_bits32: int32 -> int
