
(* todo: Int32.t *)
type t = {
  r: int8;
  g: int8;
  b: int8;
  a: int8;
}
and int8 = int

type rgba = t



(* less: sanity check between 0 and 255 *)
let mk r g b a =
  { r = r; g = g; b = b; a = a }

let mk2 r g b =
  { r = r; g = g; b = b; a = 0xFF }

let opaque = mk 0xFF 0xFF 0xFF 0xFF
let transparent = mk 0x00 0x00 0x00 0x00

let black = mk2 0x00 0x00 0x00
let white = mk2 0xFF 0xFF 0xFF

let red   = mk2 0xFF 0x00 0x00
let green = mk2 0x00 0xFF 0x00
let blue  = mk2 0x00 0x00 0xFF

let cyan    = mk2 0x00 0xFF 0xFF
let magenta = mk2 0xFF 0x00 0xFF
let yellow  = mk2 0xFF 0xFF 0x00 

(* less: cyan, magenta, ... *)

let not_a_color = mk 0xFF 0xFF 0xFF 0x00
let nofill = not_a_color

