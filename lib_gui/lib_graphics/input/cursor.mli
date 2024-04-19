open Common

type t = {
  offset: Point.t;
  clr: byte array;
  set: byte array;
}

(* for Mouse.reset_cursor *)
val arrow: t

(* helpers *)
val ints_to_bytes: int array -> byte array
