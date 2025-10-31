
val init: <Cap.draw; ..> -> string (* e.g., "rio" *) -> Display.t

val draw: 
  Image.t -> Rectangle.t -> Image.t -> Image.t option -> Point.t -> unit

val draw_color:
  Image.t -> Rectangle.t -> Image.t -> unit

val alloc_mix_colors: Display.t -> Color.t -> Color.t -> Image.t

type op =
  | SoverD

val adjust_str_for_op : string -> op -> string
