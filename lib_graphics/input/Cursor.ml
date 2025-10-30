(* Copyright 2015-2017, 2025 Yoann Padioleau, see copyright.txt *)

open Common
open Point

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mouse arrow cursor bitmap graphics *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type t = {
  offset: Point.t;

  (* of size array_size *)
  (* less: opti: could be simply a string, but ocaml allows only
   * the syntax \232 and the cursor data is usually defined using
   * hexadecimal bytes in plan9 sources, so simpler to use a 'byte array'
   *)
  clr: byte array;
  set: byte array;
}

let _array_size = 2 * 16

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* helper functions useful when building a cursor (e.g., see windows/data.ml) *)
let ints_to_bytes arr =
  arr |> Array.map (fun i ->
    if i < 0 || i > 255
    then failwith (spf "Cursor: wrong format, not a byte: %d" i);
    Char.chr i
  )

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let arrow = {
  offset = { x = -1; y = -1; };
  clr = ints_to_bytes 
    [| 0xFF; 0xFF; 0x80; 0x01; 0x80; 0x02; 0x80; 0x0C; 
       0x80; 0x10; 0x80; 0x10; 0x80; 0x08; 0x80; 0x04; 
       0x80; 0x02; 0x80; 0x01; 0x80; 0x02; 0x8C; 0x04; 
       0x92; 0x08; 0x91; 0x10; 0xA0; 0xA0; 0xC0; 0x40; 
    |];
  set = ints_to_bytes 
    [| 0x00; 0x00; 0x7F; 0xFE; 0x7F; 0xFC; 0x7F; 0xF0; 
       0x7F; 0xE0; 0x7F; 0xE0; 0x7F; 0xF0; 0x7F; 0xF8; 
       0x7F; 0xFC; 0x7F; 0xFE; 0x7F; 0xFC; 0x73; 0xF8; 
       0x61; 0xF0; 0x60; 0xE0; 0x40; 0x40; 0x00; 0x00; 
    |];
}
