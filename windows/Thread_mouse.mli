(*s: Thread_mouse.mli *)
(*s: signature [[Thread_mouse.thread]] *)
(* Reads from the mouse and sends the mouse state to the "current" window
 * if the cursor is in the windows rect. Otherwise might call the 
 * window manager to change the current window or also display a menu depending
 * on the mouse state. With this menu one can select "New" which will create
 * a new window and process (hence the need for the many Cap.xxx below).
 *)
val thread: 
  < Cap.fork; Cap.exec; Cap.chdir; Cap.mount; Cap.bind; .. > ->
  Exit.t Event.channel *
  Mouse.ctl *
  (Display.t * Baselayer.t * Image.t * Font.t) *
  Fileserver.t -> 
  unit
(*e: signature [[Thread_mouse.thread]] *)
(*e: Thread_mouse.mli *)
