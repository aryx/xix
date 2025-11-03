(*s: Mouse_action.mli *)

(*s: signature [[Mouse_action.sweep]] *)
(* allows to specify a window area *)
val sweep: 
  Mouse.ctl -> (Display.t * Baselayer.t * Font.t) -> Image.t option
(*e: signature [[Mouse_action.sweep]] *)

(*s: signature [[Mouse_action.point_to]] *)
val point_to: 
  Mouse.ctl -> Window.t option
(*e: signature [[Mouse_action.point_to]] *)
(*e: Mouse_action.mli *)
