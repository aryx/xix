(* Copyright 2026 Yoann Padioleau, see copyright.txt *)
open Common

module I = Display (* image type is in display.ml *)
module M = Draw_marshal

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let arc dst center a b thick src sp alpha phi =
  (* TODO? let alpha = 1 << 31 in *)
  Ellipse.ellipse_gen 'e' dst center a b thick src sp alpha phi Draw.SoverD

let fill dst center a b src sp alpha phi =
  Ellipse.ellipse_gen 'E' dst center a b 0 src sp alpha phi Draw.SoverD

let _arc_op dst center a b thick src sp alpha phi op =
  (* TODO? let alpha = 1 << 31 in *)
  Ellipse.ellipse_gen 'e' dst center a b thick src sp alpha phi op

let _fill_op dst center a b src sp alpha phi op =
  Ellipse.ellipse_gen 'E' dst center a b 0 src sp alpha phi op

    