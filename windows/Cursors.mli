(*s: Cursors.mli *)

(*s: signature [[Cursors.crosscursor]] *)
val crosscursor : Cursor.t
(*e: signature [[Cursors.crosscursor]] *)
(*s: signature [[Cursors.boxcursor]] *)
val boxcursor   : Cursor.t
(*e: signature [[Cursors.boxcursor]] *)
(*s: signature [[Cursors.sightcursor]] *)
val sightcursor : Cursor.t
(*e: signature [[Cursors.sightcursor]] *)
(*s: signature [[Cursors.which_corner_cursor]] *)
(*
val whitearrow: Cursor.t
val query: Cursor.t
*)

val which_corner_cursor: Rectangle.t -> Point.t -> Cursor.t
(*e: signature [[Cursors.which_corner_cursor]] *)
(*e: Cursors.mli *)
