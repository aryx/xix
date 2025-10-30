
type t = Image.t

val alloc: Baselayer.t -> Rectangle.t -> Color.t -> t

val free: t -> unit

val put_to_top: t -> unit

val put_to_bottom: t -> unit

