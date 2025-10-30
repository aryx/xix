
type op =
  | SoverD

val adjust_str_for_op : string -> op -> string

val draw: 
  Image.t -> Rectangle.t -> Image.t -> Image.t option -> Point.t -> unit

val draw_color:
  Image.t -> Rectangle.t -> Image.t -> unit

val init: string (* e.g., "rio" *) -> Display.t

val alloc_mix_colors: Display.t -> Color.t -> Color.t -> Image.t

