(* The Window Manager *)

(* main wm actions *)

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
val set_current_and_repaint:
  Window.t option -> unit

(* helpers for cursors *)

val corner_cursor_or_window_cursor: 
  Window.t -> Point.t -> Mouse.ctl -> unit

val window_cursor: 
  Window.t -> Point.t -> Mouse.ctl -> unit

(* for mutual dependencies in new_win *)
val threads_window_thread_func: (Window.t -> unit) ref
