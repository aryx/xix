
(* Thread listening to keyboard/mouse/wm events for a particular window
 * and dispatching to the right channel.
 *)
val thread: Window.t -> unit
