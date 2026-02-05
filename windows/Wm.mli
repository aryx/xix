(*s: Wm.mli *)
(* The Window Manager *)

(* main wm actions *)

(*s: signature [[Wm.new_win]] *)
(* This is called from Thread_mouse.ml when right-click and select New.
 * Internally it will:
 * - create a new Window.t and adds it to Globals.windows
 * - create a new window thread
 * - fork a new process, mount /mnt/wsys from fileserver and then binds
 *   /mnt/wsys to /dev so the process will have virtual /dev/{cons,winname,...}
 *   and finally run the cmd in it
 *)
val new_win:
  < Cap.fork; Cap.exec; Cap.chdir; Cap.mount; Cap.bind; .. > ->
  Image.t -> string (* cmd *) -> string array (* argv (including argv0) *) ->
  Fpath.t option (* pwd option *) ->
  (Mouse.ctl * Fileserver.t * Font.t) ->
  unit
(*e: signature [[Wm.new_win]] *)
(*s: signature [[Wm.close_win]] *)
val close_win:
  Window.t -> unit
(*e: signature [[Wm.close_win]] *)

(*s: signature [[Wm.hide_win]] *)
(* (those actions used to have a mouse parameter to also update the cursor) *)
val hide_win:
  Window.t ->  unit
(*e: signature [[Wm.hide_win]] *)
(*s: signature [[Wm.show_win]] *)
val show_win:
  Window.t -> Baselayer.t -> unit
(*e: signature [[Wm.show_win]] *)

(*s: signature [[Wm.top_win]] *)
val top_win:
  Window.t -> unit
(*e: signature [[Wm.top_win]] *)

(*s: signature [[Wm.resize_win]] *)
val resize_win:
  Window.t -> Image.t -> unit
(*e: signature [[Wm.resize_win]] *)

(* helpers for borders *)

(*s: signature [[Wm.set_current_and_repaint]] *)
(* this also used to have a mouse parameter (should be renamed then
 * set_current_and_repaint_borders_content_and_cursor)
 *)
val set_current_and_repaint:
  Window.t option -> unit
(*e: signature [[Wm.set_current_and_repaint]] *)

(* helpers for cursors *)

(*s: signature [[Wm.corner_cursor_or_window_cursor]] *)
val corner_cursor_or_window_cursor: 
  Window.t -> Point.t -> Mouse.ctl -> unit
(*e: signature [[Wm.corner_cursor_or_window_cursor]] *)

(*s: signature [[Wm.window_cursor]] *)
val window_cursor: 
  Window.t -> Point.t -> Mouse.ctl -> unit
(*e: signature [[Wm.window_cursor]] *)

(*s: signature [[Wm.threads_window_thread_func]] *)
(* for mutual dependencies in new_win *)
val threads_window_thread_func: (Window.t -> unit) ref
(*e: signature [[Wm.threads_window_thread_func]] *)
(*e: Wm.mli *)
