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

(* helpers *)
val int_of_buttons: buttons -> int
