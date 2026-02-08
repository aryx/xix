(* Copyright 2026 Yoann Padioleau, see copyright.txt *)
open Common

module I = Display (* image type is in display.ml *)
module M = Draw_marshal

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ellipse_gen cmd dst center xr yr thick src sp alpha phi op =
  let str = spf "%c" cmd ^ 
    M.bp_long dst.I.id ^
    M.bp_long src.I.id ^
    M.bp_point center ^
    M.bp_long xr ^ M.bp_long yr ^
    M.bp_long thick ^ M.bp_point sp ^
    M.bp_long alpha ^
    M.bp_long phi
  in
  Display.add_buf dst.I.display (Draw.adjust_str_for_op str op)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let fill dst center a b src sp =
  ellipse_gen 'E' dst center a b 0 src sp 0 0 Draw.SoverD
