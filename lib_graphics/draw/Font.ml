(* Copyright 2015-2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Point
open Rectangle

module I = Display
module FC = Fontchar
module M = Draw_marshal

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type t = {
  (* e.g., /lib/font/bit/lucm/latin1.9.font *)
  name: string (* TODO: Fpath.t*);

  (*/* max height of image, interline spacing */*)
  height : int;
  (*/* top of image to baseline */*)
  ascent: int;

  (* less: display?*)

  subfont_spec: subfont_spec list;

  cache_img: Image.t;

  (* TODO: simplified font handling: just one subfont *)
  subfont: Subfont.t option;
}
(* old: was called Cachefont in draw-C *)
and subfont_spec = {
  min_rune: Rune.t;
  max_rune: Rune.t;

  (*/* position in subfont of character at min */*)
  offset: int option;
  (*/* stored in font*/ relative filename *)
  subfont_name: string;
  (* absolute filename computed by subfontname *)
  subfont_filename: string (*TODO: Fpath.t *);
}

let _fake_font = 
  { height = -1; name = ""; ascent = 0; subfont_spec = [];
    subfont = None; cache_img = Display.fake_image 
  }

(* for default_font see font_default.ml *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* TODO: simplified to work with simplified cachec_chars and font_default.ml*)
let initialize cache_img ncache ascent =
  let display = cache_img.I.display in
  (* meh *)
  Display.flush_buffer display;
  let str = "i" ^ M.bp_long cache_img.I.id ^ M.bp_long ncache ^
    M.bp_byte ascent
  in
  Display.add_buf display str;
  Display.flush_buffer display;
  ()


(* TODO: simplified to work with simplified cache_chars and font_default.ml *)
let load_char cache_img subfont_img index fc x_fc_after =
  let display = cache_img.I.display in
  
  (* meh *)
  Display.flush_buffer display;

  (* destination coordinates in cache_img *)
  let r = { min = { x = fc.FC.xleft_in_bits; y = fc.FC.top };
            max = { x = x_fc_after; y = fc.FC.bottom } 
          } 
  in
  (* source coordinates in subfont_img *)
  let p = { x = fc.FC.xleft_in_bits; y = fc.FC.top } in

  let str = "l" ^ M.bp_long cache_img.I.id ^ M.bp_long subfont_img.I.id ^
    M.bp_short index ^ M.bp_rect r ^ M.bp_point p ^ 
    M.bp_byte fc.FC.left ^ M.bp_byte fc.FC.width in
  Display.add_buf display str
