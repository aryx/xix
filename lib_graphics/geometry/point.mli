
type t = {
  x: int;
  y: int;
}

val p: int -> int -> t

val zero: t

val add: t -> t -> t
val sub: t -> t -> t
val mul: t -> t -> t
val div: t -> t -> t
