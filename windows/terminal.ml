open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Routines to support a simple terminal emulator (the rest
 * is in Threads_window.key_in, Threads_window.runes_in, and
 * Threads_window.key_out.
 *
 * less: extract independent stuff and put in 
 * lib_graphics/ui/text_ui.ml?  or mv even more independent 
 * stuff in editor.ml somewhere?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type position = Pos of int

type t = {
  (* growing array *)
  mutable text: Rune.t array;
  (* number of runes in window *)
  mutable nrunes: int;

  (* where entered text go (and selection start) (old: q0 in rio-C) *)
  mutable cursor: position;
  mutable end_selection: position option; (* old: q1 in rio-C) *)

  (* Division between characters the host has seen and characters not 
   * yet transmitted. The position in the text that separates 
   * output from input.
   * old: qh in rio-C
   *)
  mutable output_point: position;
  (* first character visible in window from 'text' *)
  mutable origin_visible: position;

  nb_chars_visible: int;
  
  font: Font.t;
  img: Image.t;
  (* img.r without border *)
  r: Rectangle.t;

  (*
  mutable frame: Frame_ui.t;
  mutable scrollr: Rectangle.t;
  *)

}
(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let alloc img font =
  let r =
    Rectangle.insetrect Draw_rio.window_border_size img.Display.r
  in
  let nb_chars_visible = 
    raise Todo 
  in
  {
    text = [||];
    nrunes = 0;

    cursor = Pos 0;
    end_selection = None;
    output_point = Pos 0;

    origin_visible = Pos 0;

    nb_chars_visible = nb_chars_visible;
    font = font;
    img = img;
    r = r;
    (*
    frame = ();
    scrollr = Rectangle.r_empty;
    *)
  }
