(*s: Terminal.ml *)
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
 * is simpler than in Efuns though; there is no need for a gap buffer because
 * most insertions are at the end of the "file". A growing array
 * is good enough. Like in Efuns, we have also a few "cursors" that need
 * to be updated once you insert text.
 * 
 * todo:
 *  - interactive scroll bar
 *  - selection and highlited text
 * less: 
 *  - extract independent stuff and put in lib_graphics/ui/text_ui.ml?  
 *  - extract even more independent stuff in editor.ml somewhere?
 *  - take code from zed opam package?
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
(*s: type [[Terminal.position]] *)
type position = {
  i: int;
}
(*e: type [[Terminal.position]] *)
(*s: constant [[Terminal.zero]] *)
let zero = 
  { i = 0 }
(*e: constant [[Terminal.zero]] *)

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

  font: Font.t;

  (* this will alter which color to use to render the text *)
  mutable is_selected: bool;
  (* alt: mutable colors: colors; *)
  (*e: [[Terminal.t]] other fields *)
}
(*e: type [[Terminal.t]] *)

(*s: type [[Terminal.colors]] *)
type colors = {
  mutable background             : Image.t;
  mutable border                 : Image.t;
  mutable text_color             : Image.t;

  mutable background_highlighted : Image.t;
  mutable text_highlighted       : Image.t;
}
(*e: type [[Terminal.colors]] *)
(*s: constant [[Terminal.default_colors]] *)
let default_colors = {
  background             = Display.fake_image;
  border                 = Display.fake_image;
  text_color             = Display.fake_image;
  background_highlighted = Display.fake_image;
  text_highlighted       = Display.fake_image;
}
(*e: constant [[Terminal.default_colors]] *)
(*s: constant [[Terminal.dark_grey]] *)
let dark_grey = ref Display.fake_image
(*e: constant [[Terminal.dark_grey]] *)

(*s: constant [[Terminal.scrollbar_width]] *)
let scrollbar_width = 12
(*e: constant [[Terminal.scrollbar_width]] *)
(*s: constant [[Terminal.scrollbar_gap]] *)
(* gap right of scrollbar *)
let scrollbar_gap = 4
(*e: constant [[Terminal.scrollbar_gap]] *)

(*s: constant [[Terminal.tick_width]] *)
let tick_width = 3
(*e: constant [[Terminal.tick_width]] *)

(* temporary images *)
(*s: constant [[Terminal.scrollbar_img]] *)
let scrollbar_img = ref None
(*e: constant [[Terminal.scrollbar_img]] *)
(*s: constant [[Terminal.tick_img]] *)
let tick_img = ref None
(*e: constant [[Terminal.tick_img]] *)

(*****************************************************************************)
(* Debug *)
(*****************************************************************************)

(*s: constant [[Terminal.debug_keys_flag]] *)
let debug_keys_flag = ref false
(*e: constant [[Terminal.debug_keys_flag]] *)
(*s: constant [[Terminal.debug_keys_pt]] *)
let debug_keys_pt = ref Point.zero
(*e: constant [[Terminal.debug_keys_pt]] *)
(*s: function [[Terminal.debug_keys]] *)
let debug_keys term key =
  if !debug_keys_flag
  then begin
    let display = term.img.I.display in
    (* todo: why cant use display.image? does not draw ... cos desktop? *)
    let img = term.img in
    if !debug_keys_pt = Point.zero
    then debug_keys_pt := Point.sub img.I.r.max (Point.p 200 15);
    debug_keys_pt := 
      Text.string term.img !debug_keys_pt display.D.black Point.zero term.font
      (spf "%X" (Char.code key))
  end
(*e: function [[Terminal.debug_keys]] *)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)
(*s: function [[Terminal.init_colors]] *)
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
(*e: function [[Terminal.init_colors]] *)

(*s: function [[Terminal.colors_focused_window]] *)
let colors_focused_window () = 
  default_colors
(*e: function [[Terminal.colors_focused_window]] *)
(*s: function [[Terminal.colors_unfocused_window]] *)
let colors_unfocused_window () = 
  { default_colors with text_color = !dark_grey; text_highlighted = !dark_grey}
(*e: function [[Terminal.colors_unfocused_window]] *)

(*****************************************************************************)
(* Scrollbar *)
(*****************************************************************************)
(*s: function [[Terminal.scroll_pos]] *)
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
(*e: function [[Terminal.scroll_pos]] *)
  
(*s: function [[Terminal.repaint_scrollbar]] *)
let repaint_scrollbar term =
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

  let img = Fun_.once scrollbar_img (fun () ->
    let display = term.img.I.display in
    let view = display.D.image in
    (* /*factor by which window dimension can exceed screen*/ *)
    let big = 3 in
    let h = big * Rectangle.dy view.I.r in
    let r = Rectangle.r 0 0 32 h in
    Image.alloc display r view.I.chans false Color.white
  )
  in
  Draw.draw_color img r1 default_colors.border;
  Draw.draw_color img r2 default_colors.background;
  let r3 = { r2 with min = { r2.min with x = r2.max.x - 1 } } in
  Draw.draw_color img r3 default_colors.border;

  Draw.draw term.img term.scrollr img None (Point.p 0 r1.min.y)
(*e: function [[Terminal.repaint_scrollbar]] *)

(*****************************************************************************)
(* Text content *)
(*****************************************************************************)
(*s: function [[Terminal.visible_lines]] *)
let visible_lines term =
  let maxlines = Rectangle.dy term.textr / term.font.Font.height in
  
  let rec aux acc_lines acc_str nblines_done p =
    if p.i >= term.nrunes || nblines_done >= maxlines
    then Rune.string_of_runes (List.rev acc_str)::acc_lines, p
    else 
      match term.text.(p.i) with
      | '\n' -> 
        aux ((Rune.string_of_runes (List.rev acc_str))::acc_lines) []
          (nblines_done + 1) { i = p.i + 1 }
      | c ->
        aux acc_lines (c::acc_str) nblines_done { i = p.i + 1 }
  in
  let xs, lastp = aux [] [] 0 term.origin_visible in
  List.rev xs, lastp
(*e: function [[Terminal.visible_lines]] *)

(*s: function [[Terminal.repaint_content]] *)
let repaint_content term colors =
  let xs, lastp = visible_lines term in
  term.runes_visible <- lastp.i - term.origin_visible.i;

  let p = ref term.textr.min in
  xs |> List.iter (fun s ->
    let _endpt = 
      Text.string term.img !p colors.text_color Point.zero term.font s
    in
    p := { term.textr.min with y = !p.y + term.font.Font.height };
  )
(*e: function [[Terminal.repaint_content]] *)

(* helper to know if we should send runes on channel connected to an app *)
(*s: function [[Terminal.newline_after_output_point]] *)
(* "When newline, chars between output point and newline are sent."*)
let newline_after_output_point term =
  (* todo: more elegant way? way to iter over array and stop until cond? *)
  let rec aux p =
    if p.i < term.nrunes
    then 
      let c = term.text.(p.i) in
      if c = '\n'
      then true
      else aux { i = p.i + 1 }
    else false
  in
  aux term.output_point
(*e: function [[Terminal.newline_after_output_point]] *)

(*s: function [[Terminal.move_origin_to_see]] *)
(* assumes term.runes_visible is up to date, so 
 * !!do not call this function if you modified term.textr since the last 
 * repaint_content!!
 *)
let move_origin_to_see term pos =
  if pos.i >= term.origin_visible.i &&
     pos.i <= term.origin_visible.i + term.runes_visible
  then ()
  else failwith "TODO: move_origin_to_see out of range"
(*e: function [[Terminal.move_origin_to_see]] *)
  
(*****************************************************************************)
(* Tick (cursor) *)
(*****************************************************************************)
(*s: function [[Terminal.point_of_position]] *)
let point_of_position term pos =
  if pos.i >= term.origin_visible.i && 
     pos.i <=  term.origin_visible.i + term.runes_visible
  then 
    (* some of the logic below is similar in visible_lines *)
    let rec aux pt current_pos =
      if current_pos = pos
      then pt
      else
        let next_pos = { i = current_pos.i + 1 } in
        match term.text.(current_pos.i) with
        | '\n' -> 
          aux (Point.p term.textr.min.x (pt.y + term.font.Font.height)) next_pos
        | c ->
          let width = Text.string_width term.font (spf "%c" c) in
          aux { pt with x = pt.x + width } next_pos
    in
    aux term.textr.min term.origin_visible
  else 
  (* anything out of textr  *)
  term.textr.max
(*e: function [[Terminal.point_of_position]] *)

(*s: function [[Terminal.repaint_tick]] *)
let repaint_tick term colors =
  let img = Fun_.once tick_img (fun () ->
    let display = term.img.I.display in
    let view = display.D.image in
    let font = term.font in
    let height = font.Font.height in

    let r = Rectangle.r 0 0 tick_width height in
    let img = Image.alloc display r view.I.chans false Color.white in
    (*/* background color */*)
    Draw.draw_color img r colors.background;
    (*/* vertical line */*)
    Draw.draw_color img
      (Rectangle.r (tick_width / 2) 0 (tick_width / 2 + 1) height) 
      colors.text_color;
    (*/* box on each end */*)
    Draw.draw_color img (Rectangle.r 0 0 tick_width tick_width) 
      colors.text_color;
    Draw.draw_color img (Rectangle.r 0 (height - tick_width) tick_width height)
      colors.text_color;
    img
  )
  in
  let pt = point_of_position term term.cursor in
  if Rectangle.pt_in_rect pt term.textr then begin
    (*/* looks best just left of where requested */*)
    let pt = { pt with x = pt.x - 1 } in
    (*/* can go into left border but not right */*)
    let maxx = min (pt.x + tick_width) term.textr.max.x in
    let r = Rectangle.r pt.x pt.y 
                        maxx (pt.y + term.font.Font.height) 
    in
    Draw.draw term.img r img None Point.zero
  end
(*e: function [[Terminal.repaint_tick]] *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
(*s: function [[Terminal.alloc]] *)
let alloc (img : Image.t) (font : Font.t) : t =
  init_colors img.I.display;
  let r = 
    Rectangle.insetrect (Draw_rio.window_border_size + 1) img.I.r in
  let scrollr = 
    { r with max = { r.max with x = r.min.x + scrollbar_width } } in
  let textr = 
    { min = { r.min with x = scrollr.max.x + scrollbar_gap };
      (* can not use last lines if not enough pixels for a character *)
      max = { r.max with y = r.max.y - (Rectangle.dy r mod font.Font.height) };
    } in
  
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
    is_selected = true;
  }
(*e: function [[Terminal.alloc]] *)

(*s: function [[Terminal.insert_runes]] *)
(* less: return pos? used only when delete lines in term.text? *)
let insert_runes term pos runes =
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
    term.text <- Array.make (oldlen + increment) '*';
    Array.blit old 0 term.text 0 oldlen;
  end;

  assert (pos.i <= term.nrunes);
  (* move to the right the runes after the cursor pos to make some space *)
  Array.blit term.text pos.i term.text (pos.i + n) (term.nrunes - pos.i);
  (* fill the space *)
  runes |> List.iteri (fun i rune ->
    term.text.(pos.i + i) <- rune;
  );
  term.nrunes <- term.nrunes + n;

  (* adjust cursors *)
  if pos.i <= term.cursor.i 
  then term.cursor <- { i = term.cursor.i + n };
  term.end_selection |> Option.iter (fun ends ->
    if pos.i <= ends.i
    then term.end_selection <- Some ({ i = ends.i + n});
  );
  (* note the '<' here, not '<=' ! *)
  if pos.i < term.output_point.i
  then term.output_point <- { i = term.output_point.i + n };
  if pos.i < term.origin_visible.i
  then term.origin_visible <- { i = term.origin_visible.i + n };
  ()
(*e: function [[Terminal.insert_runes]] *)

(*s: function [[Terminal.delete_runes]] *)
let delete_runes term pos n =
  (* stricter: *)
  if n = 0
  then failwith "delete_runes: empty delete";
  let pos2 = { i = pos.i + n } in

  Array.blit term.text pos2.i term.text pos.i (term.nrunes - pos2.i);
  term.nrunes <- term.nrunes - n;

  (* adjust cursors *)
  if pos.i < term.cursor.i
  then term.cursor <- { i = term.cursor.i - min n (term.cursor.i - pos.i) };
  term.end_selection |> Option.iter (fun ends ->
    if pos.i < ends.i
    then term.end_selection <- Some ({ i = ends.i - min n (ends.i - pos.i)});
  );

  (match () with
  | _ when term.output_point.i > pos2.i ->
    term.output_point <- { i = term.output_point.i - n };
  | _ when term.output_point.i > pos.i -> 
    term.output_point <- pos;
  | _ -> assert(term.output_point.i <= pos.i);
    ()
  );

  if pos2.i <= term.origin_visible.i
  then term.origin_visible <- { i = term.origin_visible.i -n };
  ()
(*e: function [[Terminal.delete_runes]] *)
  

(*s: function [[Terminal.repaint]] *)
let repaint term =
  let colors = 
    if term.is_selected
    then colors_focused_window ()
    else colors_unfocused_window ()
  in

  (* start from scratch with blank image, otherwise tick will be
   * paint all over the place.
   * alt: save what was under the tick image.
   *)
  if not !debug_keys_flag
  then Draw.draw_color term.img term.r colors.background;

  (* repaint_content() needs to be before repaint_scrollbar() because it
   * updates term.runes_visible used by repaint_scrollbar().
   *)
  repaint_content term colors;
  repaint_scrollbar term;
  repaint_tick term colors;
  ()
(*e: function [[Terminal.repaint]] *)

(*****************************************************************************)
(* External events *)
(*****************************************************************************)

(* todo: right now 'key' is a byte (a char), but it should be a rune.
 * So for now we need to reconstruct a rune from a series of chars
 * (the utf8 encoding of the rune).
 *)
(*s: constant [[Terminal.previous_code]] *)
let previous_code = ref 0
(*e: constant [[Terminal.previous_code]] *)

(*s: function [[Terminal.key_in]] *)
let key_in term key =
  (match Char.code key with

  (* less: navigation keys are handled in the caller in rio-C, to allow keys
   * navigation even in raw mode, but it's more cohesive to put such
   * code here.
   *)

  (* Left arrow *)
  | 0x91 when !previous_code = 0x80 (* less: and previous_previous = 0xEF *) ->
    let cursor = term.cursor in
    if cursor.i > 0
    then term.cursor <- { i = cursor.i - 1 };
    move_origin_to_see term term.cursor
    
  (* Right arrow *)
  | 0x92 when !previous_code = 0x80 ->
    (* less: use end_position cursor instead? *)
    let cursor = term.cursor in
    if cursor.i < term.nrunes
    then term.cursor <- { i = cursor.i + 1 };
    move_origin_to_see term term.cursor

  (* End of line ^E *)
  | 0x5 ->
    (* alt: have term.newlines as in Efuns so easier *)
    let rec find_eol_or_end pos = 
      if pos.i = term.nrunes || term.text.(pos.i) = '\n'
      then pos
      else find_eol_or_end { i = pos.i + 1 }
    in
    term.cursor <- find_eol_or_end term.cursor;
    move_origin_to_see term term.cursor
  (* Beginning of line ^A *)
  | 0x1 ->
    (* alt: have term.newlines as in Efuns so easier *)
    let rec find_bol_or_start pos = 
      if pos.i = 0 || pos.i = term.output_point.i ||
         term.text.(pos.i - 1) = '\n'
      then pos
      else find_bol_or_start { i = pos.i - 1 }
    in
    term.cursor <- find_bol_or_start term.cursor;
    move_origin_to_see term term.cursor


  (* Erase character ^H  (or Backspace or Delete) *)
  | 0x8 ->
    let cursor = term.cursor in
    if cursor.i = 0 || cursor = term.output_point
    then ()
    else begin
      (* less: adjust if cursor = term.origin_visible *)
      (* this will adjust term.cursor *)
      delete_runes term { i = cursor.i - 1 } 1
    end

  (* todo: should remove once key is directly a rune.
   * less: the sequence we get for Left arrow is really
   * 0xEF 0x80 0x91, but we just check if previous code is 0x80.
   *)
  | 0xEF -> ()
  | 0x80 -> ()

  (* ordinary characters *)
  | _ ->
    debug_keys term key;
    (* this will adjust term.cursor *)
    insert_runes term term.cursor [key] 
  );
  previous_code := Char.code key;
  if !previous_code = 0xEF || !previous_code = 0x80
  then ()
  else 
  repaint term
(*e: function [[Terminal.key_in]] *)

(*s: function [[Terminal.runes_in]] *)
(* "when characters are sent from the host, they are inserted at
 * the output point and the output point is advanced."
 *)
let runes_in term runes =
  insert_runes term term.output_point runes;
  term.output_point <- { i = term.output_point.i + List.length runes };
  repaint term;
  ()
(*e: function [[Terminal.runes_in]] *)

(* "When newline, chars between output point and newline are sent."
 * let bytes_out term =
 * but the logic is in Threads_window.bytes_out to factorize code
 * with the logic when in raw-mode.
 *)
(*e: Terminal.ml *)
