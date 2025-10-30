
type end_line =
  | EndSquare
  | EndDisc
  | EndArrow

val line: 
  Image.t -> Point.t -> Point.t -> end_line -> end_line -> int -> Image.t ->
  Point.t -> unit

