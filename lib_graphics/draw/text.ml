open Common

module I = Image

let string_gen dst pt src sp ft str_or_rune len clipr bg bgp op =
  raise Todo

let string dst pt src sp ft str =
  string_gen dst pt src sp ft (Left str) (String.length str) 
    dst.I.clipr None Point.zero Draw.SoverD
