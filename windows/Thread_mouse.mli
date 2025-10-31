
(* Reads from the mouse and sends the mouse state to the "current" window
 * if the cursor is in the windows rect. Otherwise might call the 
 * window manager to change the current window or also display a menu depending
 * on the mouse state.
 *)
val thread: 
  Exit.t Event.channel * Mouse.ctl * 
  (Display.t * Baselayer.t * Image.t * Font.t) * Fileserver.t -> 
  unit
