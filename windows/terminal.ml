open Common

open Point
open Rectangle

module I = Display
module D = Display

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Routines to support a simple terminal emulator.
 *  
 * A terminal has many features in common with an editor: you can enter
 * text, move around the cursor, copy, cut, paste, etc. The situation
 * is simpler than in Efuns though. There is no need for a gap buffer because
 * most insertions are at the end of the "file". A growing array
 * is good enough. Like in Efuns, we have a few "cursors" that need
 * to be updated once you insert text.
 * 
 * todo:
 *  - interactive scroll bar
 *  - selection and highlited text
 * less: 
 *  - extract independent stuff and put in lib_graphics/ui/text_ui.ml?  
 *  - extract even more independent stuff in editor.ml somewhere?
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)

(* The type below is called a 'point' in Efuns, but it would be
 * confusing with the Point.t of lib_graphics/geometry/point.ml.
 * We could also call it 'cursor', but this would be 
 * confusing with the Cursor.t of lib_graphics/input/cursor.ml
 * 
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

  (* less: lines? like in Efuns? with EOF sentinel to simplify code? *)

  (* where entered text go (and selection start) (old: q0 in rio-C) *)
  mutable cursor: position;
  mutable end_selection: position option; (* old: q1 in rio-C) *)

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

  (* todo? colors? *)
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
let dark_grey = ref Display.fake_image

let scrollbar_width = 12
(* gap right of scrollbar *)
let scrollbar_gap = 4

let scrollbar_temp = ref Display.fake_image

(*****************************************************************************)
(* Colors *)
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

    (*/* greys are multiples of 0x11111100+0xFF, 14* being palest */*)
    dark_grey := Image.alloc_color display (Color.mk2 0x66 0x66 0x66);
  end

let colors_focused_window () = 
  default_colors
let colors_unfocused_window () = 
  { default_colors with text_color = !dark_grey; text_highlighted = !dark_grey}

(*****************************************************************************)
(* Scrollbar *)
(*****************************************************************************)

let init_scrollbar display =
  if !scrollbar_temp == Display.fake_image
  then begin
    (* /*factor by which window dimension can exceed screen*/ *)
    let big = 3 in
    let view = display.D.image in
    let h = big * Rectangle.dy view.I.r in
    let r = Rectangle.r 0 0 32 h in
    scrollbar_temp := Image.alloc display r view.I.chans false Color.white;
  end
  

let scroll_pos r p0 p1 total =
  if total = 0 
  then r 
  else
    let h = Rectangle.dy r in
    let miny = r.min.y + 
      if p0.i > 0
      then p0.i * h / total
      else 0
    in
    let maxy = r.max.y - 
      if p1.i < total
      then (total - p1.i) * h / total
      else 0
    in
    let maxy, miny =
      if maxy < miny + 2
      then 
        if miny+2 <= r.max.y
        then miny+2, miny
        else maxy, maxy-2
      else maxy, miny
    in
    Rectangle.r r.min.x miny r.max.x maxy
    
  
let repaint_scrollbar term =
  init_scrollbar term.img.I.display;
  let r = term.scrollr in
  
  let v = Point.p (Rectangle.dx r) 0 in
  let r1 = Rectangle.sub_pt v r in
  let r2 = scroll_pos r1 
    term.origin_visible 
    { i = term.origin_visible.i + term.runes_visible }
    term.nrunes
  in
  let r2 = Rectangle.sub_pt r1.min r2 in
  let r1 = Rectangle.sub_pt r1.min r1 in
  let tmp = !scrollbar_temp in
  Draw.draw_color tmp r1 default_colors.border;
  Draw.draw_color tmp r2 default_colors.background;
  let r3 = { r2 with min = { r2.min with x = r2.max.x - 1 } } in
  Draw.draw_color tmp r3 default_colors.border;

  Draw.draw term.img term.scrollr tmp None (Point.p 0 r1.min.y)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let alloc img font =
  init_colors img.I.display;
  let r = 
    Rectangle.insetrect (Draw_rio.window_border_size + 1) img.I.r in
  let scrollr = 
    { r with max = { r.max with x = r.min.x + scrollbar_width } } in
  let textr = 
    { r with min = { r.min with x = r.min.x + scrollbar_width+scrollbar_gap}} in
  (* less: remove bottom line? *)
  
  {
    text = [||];
    nrunes = 0;

    cursor = zero;
    end_selection = None;
    output_point = zero;

    origin_visible = zero;
    runes_visible = 0;

    font = font;
    img = img;
    r = r;
    textr = textr;
    scrollr = scrollr;
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

let repaint term _is_selected =
  repaint_scrollbar term
  


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
