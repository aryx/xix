(*s: Terminal.mli *)
(*s: type [[Terminal.position]] *)
type position = {
  i: int;
}
(*e: type [[Terminal.position]] *)

(*s: type [[Terminal.t]] *)
type t = {
  (* the model *)

  (*s: [[Terminal.t]] text data fields *)
  (* growing array (simpler than a gap buffer).
   * alt: a growing string, like in Efuns.
   *)
  mutable text: Rune.t array;
  (* number of runes used in text (<= Array.length term.text) *)
  mutable nrunes: int;

  (* less: lines? like in Efuns? with EOF sentinel to simplify code? *)
  (*e: [[Terminal.t]] text data fields *)
  (*s: [[Terminal.t]] text cursor fields *)
  (* where entered text go (and selection start) (old: q0 in rio) *)
  mutable cursor: position;
  mutable end_selection: position option;      (* old: q1 in rio) *)
  (*x: [[Terminal.t]] text cursor fields *)
  (* Division between characters the host has seen and characters not 
   * yet transmitted. The position in the text that separates output from input.
   * old: qh in rio
   *)
  mutable output_point: position;
  (*e: [[Terminal.t]] text cursor fields *)

  (* the view *)

  (*s: [[Terminal.t]] graphics fields *)
  img: Image.t;

  (* img.r without border and some extra space *)
  r: Rectangle.t;
  (* right side of r, and no bottom rectangle covering a line of
   * text with font below 
   *)
  textr: Rectangle.t;
  (* left side of r *)
  scrollr: Rectangle.t;
  (*e: [[Terminal.t]] graphics fields *)

  (* misc *)

  (*s: [[Terminal.t]] other fields *)
  (* first character visible in window from 'text' *)
  mutable origin_visible: position;
  mutable runes_visible: int;
  (*x: [[Terminal.t]] other fields *)
  font: Font.t;
  (*x: [[Terminal.t]] other fields *)
  (* this will alter which color to use to render the text *)
  mutable is_selected: bool;
  (*x: [[Terminal.t]] other fields *)
  (* alt: mutable colors: colors; *)
  (*e: [[Terminal.t]] other fields *)
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
