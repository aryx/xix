open Common
open Point

module I = Image

let string_gen dst pt src sp ft str_or_rune len clipr bg bgp op =
  failwith "string_gen: Todo"

let string dst pt src sp font str =
(*
  string_gen dst pt src sp ft (Left str) (String.length str) 
    dst.I.clipr None Point.zero Draw.SoverD
*)
  Draw.draw dst
    (Rectangle.r 
       pt.x 
       pt.y 
       (pt.x + Font.string_width font str) 
       (pt.y + font.Font.height)
    )
    src None sp

