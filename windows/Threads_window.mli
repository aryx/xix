(*s: Threads_window.mli *)
(*s: signature [[Threads_window.thread]] *)
(* Thread listening to keyboard/mouse/wm events for a particular window
 * and dispatching to the right channel.
 *)
val thread: Window.t -> unit
(*e: signature [[Threads_window.thread]] *)
(*e: Threads_window.mli *)
