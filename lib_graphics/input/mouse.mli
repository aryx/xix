open Common

type state = {
  pos: Point.t;
  buttons: buttons;
  msec: int;
}
  and buttons = { left: bool; middle: bool; right: bool; }

val nobuttons: buttons

type button = Left | Middle | Right

val has_click: state -> bool
val has_button: state -> button -> bool

val mk: Point.t -> button -> state

val fake_state: state

(* helpers *)
val int_of_buttons: buttons -> int

type ctl = {
  (* /dev/mouse *)
  fd: Unix.file_descr;
  (* streams of mouse events that can be received from thread_mouse below *)
  chan: state Event.channel;
  (* /dev/cursor *)
  cursor_fd: Unix.file_descr;
}


(* will create a mouse thread reading ctl.fd and sending mouse state on
 * ctl.chan
 *)
val init: unit -> ctl

val receive: ctl -> state Event.event
val read: ctl -> state
val flush_and_read: Display.t -> ctl -> state

val set_cursor: ctl -> Cursor.t -> unit
val reset_cursor: ctl -> unit

