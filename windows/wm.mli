
(* main wm actions *)

val new_win:
  Image.t -> string -> string array -> Common.filename option ->
  (Mouse.ctl * Fileserver.t * Font.t) ->
  unit
val close_win:
  Window.t -> unit

(* (those actions used to have a mouse parameter to also update the cursor) *)
val hide_win:
  Window.t ->  unit
val show_win:
  Window.t -> Baselayer.t -> unit

val top_win:
  Window.t -> unit

val resize_win:
  Window.t -> Image.t -> unit


(* helpers for borders *)

(* this also used to have a mouse parameter (should be renamed then
 * set_current_and_repaint_borders_content_and_cursor)
 *)
val set_current_and_repaint_borders:
  Window.t option -> unit

(* helpers for cursors *)

val corner_cursor_or_window_cursor: 
  Window.t -> Point.t -> Mouse.ctl -> unit

val window_cursor: 
  Window.t -> Point.t -> Mouse.ctl -> unit

(* for mutual dependencies in new_win *)
val threads_window_thread_func: (Window.t -> unit) ref
