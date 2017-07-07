type position = {
  i: int;
}

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

val alloc: Image.t -> Font.t -> t

val key_in: t -> Keyboard.key -> unit
val runes_in: t -> Rune.t list -> unit

val newline_after_output_point: t -> bool

val repaint: t -> unit
