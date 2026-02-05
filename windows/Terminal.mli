(*s: Terminal.mli *)
(*s: type [[Terminal.position]] *)
type position = {
  i: int;
}
(*e: type [[Terminal.position]] *)

(*s: type [[Terminal.t]] *)
type t = {
  (* the model *)

  (* growing array (simpler than a gap buffer).
   * alt: a growing string, like in Efuns.
   *)
  mutable text: Rune.t array;
  (* number of runes used in text (<= Array.length term.text) *)
  mutable nrunes: int;

  (* less: lines? like in Efuns? with EOF sentinel to simplify code? *)

  (* where entered text go (and selection start) (old: q0 in rio-C) *)
  mutable cursor: position;
  mutable end_selection: position option;      (* old: q1 in rio-C) *)

  (* Division between characters the host has seen and characters not 
   * yet transmitted. The position in the text that separates output from input.
   * old: qh in rio-C
   *)
  mutable output_point: position;

  (* the view *)

  img: Image.t;

  (* img.r without border and some extra space *)
  r: Rectangle.t;
  (* right side of r, and no bottom rectangle covering a line of
   * text with font below 
   *)
  textr: Rectangle.t;
  (* left side of r *)
  scrollr: Rectangle.t;

  (* first character visible in window from 'text' *)
  mutable origin_visible: position;
  mutable runes_visible: int;

  font: Font.t;

  (* this will alter which color to use to render the text *)
  mutable is_selected: bool;
  (* alt: mutable colors: colors; *)
}
(*e: type [[Terminal.t]] *)

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
