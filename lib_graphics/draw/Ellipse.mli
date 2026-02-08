
val ellipse:
  Image.t -> Point.t -> int -> int -> int -> Image.t -> Point.t -> unit

val fill:
  Image.t -> Point.t -> int -> int -> Image.t -> Point.t -> unit

(* internals used also in Arc *)
val ellipse_gen:
  char -> Image.t -> Point.t -> int -> int -> int -> Image.t -> Point.t ->
  int -> int -> Draw.op -> unit

