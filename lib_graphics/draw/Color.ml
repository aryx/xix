(* Copyright 2015-2026 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* less: opti: Int32.t *)
type t = {
  r: int8;
  g: int8;
  b: int8;
  a: int8;
}
and int8 = int

type rgba = t


(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* less: sanity check between 0 and 255 *)
let mk r g b a =
  { r = r; g = g; b = b; a = a }

let mk2 r g b = 
  mk r g b 0xFF

let opaque = mk 0xFF 0xFF 0xFF 0xFF
let transparent = mk 0x00 0x00 0x00 0x00

(* no color *)
let black = mk2 0x00 0x00 0x00
(* all colors *)
let white = mk2 0xFF 0xFF 0xFF

let red   = mk2 0xFF 0x00 0x00
let green = mk2 0x00 0xFF 0x00
let blue  = mk2 0x00 0x00 0xFF

let cyan    = mk2 0x00 0xFF 0xFF
let magenta = mk2 0xFF 0x00 0xFF
let yellow  = mk2 0xFF 0xFF 0x00 

(* less: see pfff/.../emacs_colors.ml *)
let darkgreen = mk2 0x44 0x88 0x44
let palegreen = mk2 0xAA 0xFF 0xAA
let medgreen = mk2 0x88 0xCC 0x88

let darkred = mk2 0x88 0x44 0x44
let palered = mk2 0xFF 0xAA 0xAA
let medred  = mk2 0xCC 0x88 0x88

let greygreen = mk2 0x55 0xAA 0xAA
let palegreygreen = mk2 0x9E 0xEE 0xEE

let not_a_color = mk 0xFF 0xFF 0xFF 0x00
let nofill = not_a_color

