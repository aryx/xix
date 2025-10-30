
type t = {
  min: Point.t;
  (* 'max' does not belong to the rectangle, so a rectangle from (0,0) to
   * (100,100) is actually going until (99,99). The rectangle length and
   * width are 100 though.
   *)
  max: Point.t;
}

val r_empty:  t
val r_1x1 : t

val r: int -> int -> int -> int -> t

val canonical: Point.t -> Point.t -> t

val rp: Point.t -> Point.t -> t

val dx: t -> int
val dy: t -> int

val add_pt: Point.t -> t -> t
val sub_pt: Point.t -> t -> t

val pt_in_rect: Point.t -> t -> bool
val rect_in_rect: t -> t -> bool

val insetrect: int -> t -> t
