(*s: Terminal.mli *)
(*s: type [[Terminal.position (Terminal.mli)]] *)
type position = {
  i: int;
}
(*e: type [[Terminal.position (Terminal.mli)]] *)

(*s: type [[Terminal.t (Terminal.mli)]] *)
type t = {
  (* the model *)

  mutable text: Rune.t array;
  mutable nrunes: int;

  mutable cursor: position;
  mutable end_selection: position option;

  mutable output_point: position;

  (* the view *)

  img: Image.t;

  r: Rectangle.t;
  textr: Rectangle.t;
  scrollr: Rectangle.t;

  mutable origin_visible: position;
  mutable runes_visible: int;

  font: Font.t;

  mutable is_selected: bool;
}
(*e: type [[Terminal.t (Terminal.mli)]] *)

(*s: signature [[Terminal.alloc]] *)
val alloc: Image.t -> Font.t -> t
(*e: signature [[Terminal.alloc]] *)

(*s: signature [[Terminal.key_in]] *)
val key_in: t -> Keyboard.key -> unit
(*e: signature [[Terminal.key_in]] *)
(*s: signature [[Terminal.runes_in]] *)
val runes_in: t -> Rune.t list -> unit
(*e: signature [[Terminal.runes_in]] *)

(*s: signature [[Terminal.newline_after_output_point]] *)
val newline_after_output_point: t -> bool
(*e: signature [[Terminal.newline_after_output_point]] *)

(*s: signature [[Terminal.repaint]] *)
val repaint: t -> unit
(*e: signature [[Terminal.repaint]] *)
(*e: Terminal.mli *)
