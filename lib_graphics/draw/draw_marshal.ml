open Common
open Point
open Rectangle
open Color

(* todo: sanity check range *)
let bp_byte x =
  String.make 1 (Char.chr x)

let bp_short x =
  (* less: sanity check range? *)
  let x1 = Char.chr (x land 0xFF) in
  let x2 = Char.chr ((x asr 8) land 0xFF) in
  let str = Bytes.make 2 ' ' in
  Bytes.set str 0 x1;
  Bytes.set str 1 x2;
  Bytes.to_string str

(* less: use commons/byte_order.ml? *)
let bp_long x =
  let x1 = Char.chr (x land 0xFF) in
  let x2 = Char.chr ((x asr 8) land 0xFF) in
  let x3 = Char.chr ((x asr 16) land 0xFF) in
  let x4 = Char.chr ((x asr 24) land 0xFF) in
  let str = Bytes.make 4 ' ' in
  Bytes.set str 0 x1;
  Bytes.set str 1 x2;
  Bytes.set str 2 x3;
  Bytes.set str 3 x4;
  Bytes.to_string str

let bp_bool b =
  if b
  then String.make 1 (Char.chr 1)
  else String.make 1 (Char.chr 0)

let bp_point pt =
  bp_long pt.x ^ bp_long pt.y

let bp_rect r =
  bp_long r.min.x ^ bp_long r.min.y ^ 
  bp_long r.max.x ^ bp_long r.max.y


let bp_color c =
  bp_byte c.a ^ bp_byte c.b ^ bp_byte c.g ^ bp_byte c.r

let bp_chans chans =
  let i = Channel.mk_channels_serial chans in
  bp_long i


(* alt:
 * marshall full AST if kernel was written in OCaml:
 * 
type msg =
  | AllocImage of int

  | Draw of int

  | Line of int
  | Ellipse of int
  | Arc of int
 *)
