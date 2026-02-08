
type t = {
  x: int;
  y: int;
}

val p: int -> int -> t

val zero: t

val add: t -> t -> t
val sub: t -> t -> t

val mul: t -> int -> t
val div: t -> int -> t
