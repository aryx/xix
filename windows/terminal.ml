open Common

module I = Display
module D = Display

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Routines to support a simple terminal emulator.
 *  
 * A terminal has many features in common with an editor. You can enter
 * text, move around the cursor, copy, cut, paste, etc. The situation
 * is simpler than in Efuns though. We need less a gap buffer because
 * most insertions are at the end of the "file". A growing array
 * is good enough. Like in Efuns, we have a few "cursors" that need
 * to be updated once you insert text.
 * 
 * todo:
 *  - scroll bar
 *  - selection and highlited text
 * less: 
 *  - extract independent stuff and put in lib_graphics/ui/text_ui.ml?  
 *  - extract even more independent stuff in editor.ml somewhere?
 *)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

(* The type below is called a 'point' in Efuns, but it would be
 * confusing with the Point.t of lib_graphics/geometry.
 * less: make mutable instead of the fields in 't' below?
 *)
type position = {
  i: int;
}
let zero = 
  { i = 0 }

type t = {
  (* the model *)

  (* growing array (simpler than a gap buffer).
   * alt: a growing string, like in Efuns.
   *)
  mutable text: Rune.t array;
  (* number of runes used in text *)
  mutable nrunes: int;

  (* less: lines? like in Efuns? *)

  (* where entered text go (and selection start) (old: q0 in rio-C) *)
  mutable cursor: position;
  mutable end_selection: position option; (* old: q1 in rio-C) *)

  (* Division between characters the host has seen and characters not 
   * yet transmitted. The position in the text that separates 
   * output from input.
   * old: qh in rio-C
   *)
  mutable output_point: position;

  (* the view *)

  img: Image.t;
  (* img.r without border and some extra space *)
  r: Rectangle.t;
  (* r without a bottom rectangle covering a line of text with font below *)
  textr: Rectangle.t;

  (* first character visible in window from 'text' *)
  mutable origin_visible: position;

  font: Font.t;

  (* todo? colors? *)

  (* todo:
  mutable frame: Frame_ui.t;
  mutable scrollr: Rectangle.t;
  *)
}

type colors = {
  mutable background             : Image.t;
  mutable border                 : Image.t;
  mutable text_color             : Image.t;
  mutable background_highlighted : Image.t;
  mutable text_highlighted       : Image.t;
}
let default_colors = {
  background             = Display.fake_image;
  border                 = Display.fake_image;
  text_color             = Display.fake_image;
  background_highlighted = Display.fake_image;
  text_highlighted       = Display.fake_image;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let init_colors display =
  if default_colors.background == Display.fake_image
  then begin
    default_colors.background <- display.D.white;
    default_colors.background_highlighted <-
      Image.alloc_color display (Color.mk2 0xCC 0xCC 0xCC);
    default_colors.border <-
      Image.alloc_color display (Color.mk2 0x99 0x99 0x99);
    default_colors.text_color <- display.D.black;
    default_colors.text_highlighted <- display.D.black;
  end

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let alloc img font =
  init_colors img.I.display;
  let r =
    Rectangle.insetrect (Draw_rio.window_border_size + 1) img.I.r
  in
  (* less: remove bottom line *)
  let textr = r in
  {
    text = [||];
    nrunes = 0;

    cursor = zero;
    end_selection = None;
    output_point = zero;

    origin_visible = zero;
    font = font;
    img = img;
    r = r;
    textr = textr;
    (*
    frame = ();
    scrollr = Rectangle.r_empty;
    *)
  }

(* less: return pos? used only when delete lines in term.text? *)
let insert_runes term runes pos =
  let n = List.length runes in
  (* stricter: *)
  if n = 0
  then failwith "insert_runes: empty runes";

  (* grow array if necessary *)
  if term.nrunes + n > Array.length term.text then begin
    let old = term.text in
    let oldlen = Array.length old in
    (* todo: high_water, low_water, etc *)
    let increment = 20000 in
    term.text <- Array.create (oldlen + increment) '*';
    Array.blit old 0 term.text 0 oldlen;
  end;

  assert (pos.i <= term.nrunes);
  (* move to the right the runes after the cursor pos to make some space *)
  Array.blit term.text pos.i term.text (pos.i + n) (term.nrunes - pos.i);
  (* fill the space *)
  runes |> List_.iteri (fun i rune ->
    term.text.(pos.i + i) <- rune;
  );
  term.nrunes <- term.nrunes + n;

  (* adjust cursors *)
  if pos.i <= term.cursor.i 
  then term.cursor <- { i = term.cursor.i + n };
  term.end_selection |> Common.if_some (fun ends ->
    if pos.i <= ends.i
    then term.end_selection <- Some ({ i = ends.i + n});
  );
  (* note the '<' here, not '<=' ! *)
  if pos.i < term.output_point.i
  then term.output_point <- { i = term.output_point.i + n };

  (match () with
  | _ when pos.i < term.origin_visible.i ->
    term.origin_visible <- { i = term.origin_visible.i + n }
(*
  | _ when pos.i <= term.origin_visible.i + term.nbrunes_visible ->
    (* todo: frinsert *)
    ()
*)
  | _ -> ()
  )

(*
let show_pos term pos =
  let endpos = term.origin_visible.i + term.nbrunes_visible in
  if term.origin_visible.i <= pos.i && 
     (pos.i < endpos || (pos.i = endpos && endpos = term.nrunes))
  then 
    draw term
  else
    (* pos out of scope *)
    failwith "TODO: show_pos out of scope"
*)

(*****************************************************************************)
(* External events *)
(*****************************************************************************)
(* "When newline, chars between output point and newline are sent."*)
let key_in term key =
  failwith "Terminal.key_in: TODO"


(* "when characters are sent from the host, they are inserted at
 * the output point and the output point is advanced."
 *)
let runes_in term runes =
(*  
  let pt = ref w.screenr.min in
  runes |> List.iter (fun rune ->
    pt := Text.string w.img !pt !Globals.red Point.zero w.terminal.T.font 
      (String.make 1 rune);
  );
  ()
*)
  ()

let bytes_out term =
  failwith "Terminal.bytes_out: TODO"
