open Common
open Point
open Rectangle

module I = Display (* image type is in display.ml *)
module D = Display

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* old: called menuhit.c in draw-C
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)


(* inspiration is GToolbox in lablgtk *)
type item = (string * (unit -> unit))
type items = item list

(* outside   | outline | space |  text ...
 *           <-----------------> 
 *               margin
 *           <--------->
 *            border size
 *)
let margin     = 4 (* outside to text *)
let border_size = 2 (* width of outlining border *)
let vspacing   = 2 (* extra spacing between lines of text *)

let background             = ref Display.fake_image
let background_highlighted = ref Display.fake_image
let border_color           = ref Display.fake_image
let text_color             = ref Display.fake_image
let text_highlighted       = ref Display.fake_image
(*let menutext        = ref Display.fake_image *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let init_colors display =
  if !background == Display.fake_image
  then begin
    (* less: could use try and default to black/white if can not alloc image*)
    (* less: opti: use view->chan instead of rgb24 used by Image.alloc_color *)
    background := Draw.alloc_mix_colors display Color.palegreen Color.white;
    background_highlighted := Image.alloc_color display (Color.darkgreen);
    border_color           := Image.alloc_color display (Color.medgreen);
    text_color       := display.D.black;
    text_highlighted := !background;
  end

(* "Text is the rectangle holding the text elements.
 * Return the rectangle, including its black edge, holding element i."
 *)
let rect_of_menu_entry textr i font =
  if i < 0
  then failwith (spf "rect_of_menu_entry with negative entry %d" i);
  let line_height = font.Font.height + vspacing in
  let rminy = textr.min.y + i * line_height in
  let r = { min = { textr.min with y = rminy };
            max = { textr.max with y = rminy + line_height };
          }
  in
  (* include black edge for selection *)
  Rectangle.insetrect (border_size - margin) r

type action =
  | SaveOn of Image.t
  | RestoreFrom of Image.t
  | Nothing

let paint_item (img:Image.t) (font:Font.t) (i:int) (entries: string array) textr highlight action =
  let line_height = font.Font.height + vspacing in
  let r = rect_of_menu_entry textr i font in
  let str = entries.(i) in
  
  (match action with
  | Nothing -> ()
  | SaveOn      save -> Draw.draw save save.I.r img None r.min 
  | RestoreFrom save -> Draw.draw img r save None save.I.r.min
  );
  let pt = { x = (textr.min.x + textr.max.x - Text.string_width font str) / 2;
             y = textr.min.y + i * line_height 
           }
  in
  Draw.draw img r 
    (if highlight then !background_highlighted else !background) None 
    (*pt??*) Point.zero;
  Text.string img pt 
    (if highlight then !text_highlighted else !text_color) 
    (*pt??*) Point.zero font str;
  ()

let paint_item_opt img font iopt entries textr highlight action =
  iopt |> Common.if_some (fun i ->
    paint_item img font i entries textr highlight action
  )

let menu_selection font textr p =
  let line_height = font.Font.height + vspacing in
  (* todo? insetrect Margin? *)
  let r = textr in
  if not (Rectangle.pt_in_rect p r)
  then None
  else
    let i = (p.y - r.min.y) / line_height in
    Some i


let scan_items img font  mouse button  iopt entries textr  save =
  paint_item_opt img font  iopt entries textr true (SaveOn save);
  let rec loop_while_button m iopt =
    if Mouse.has_button m button
    then 
    let i_opt = menu_selection font textr m.Mouse.pos in
    (match i_opt with
    | Some i when (Some i) = iopt -> 
      loop_while_button (Mouse.flush_and_read img.I.display mouse) iopt
    | None -> 
      paint_item_opt img font iopt entries textr  false (RestoreFrom save);
      None
    | Some i ->
      paint_item_opt img font  iopt entries textr  false(RestoreFrom save);
      let iopt = Some i in
      paint_item_opt img font iopt entries textr  true (SaveOn save);
      loop_while_button (Mouse.flush_and_read img.I.display mouse) iopt
    )
    else iopt
  in
  loop_while_button (Mouse.flush_and_read img.I.display mouse) iopt
      


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let menu items pos button mouse (display, desktop, view, font) =
  init_colors display;
  (* less: reset clipr and repl on view? *)

  (* compute width and height and basic config *)

  let max_width = 
    items |> List.map (fun (str, _f) -> Text.string_width font str)
          |> List.fold_left max 0
  in
  (* todo: if scrolling *)
  let width = max_width in
  let nitems_to_draw = List.length items in
  let entries = items |> List.map fst |> Array.of_list in
  (* less: save lasthit between calls to menu and restore it here *)
  let lasti = 0 in

  let line_height = font.Font.height + vspacing in
  let height = nitems_to_draw * line_height in

  (* compute rectangles *)

  let r = 
    Rectangle.r 0 0 width height 
    |> Rectangle.insetrect (-margin)
    (* center on lasti entry *)
    |> Rectangle.sub_pt
        (Point.p (width / 2) (lasti * line_height + font.Font.height / 2))
    (* adjust to mouse position *)
    |> Rectangle.add_pt pos
  in
  
  (* less: adjust if outside view *)
  let pt_adjust = Point.zero in
  let menur = Rectangle.add_pt pt_adjust r in

  (* less: do the more complex calculation?*)
  let textr = Rectangle.insetrect margin menur in

  (* set images to draw on *)

  (* less: handle case where no desktop? *)
  let img = Layer.alloc desktop menur Color.white  in
  let save = 
    Image.alloc display (rect_of_menu_entry textr 0 font) view.I.chans false
      Color.black
  in

  (* painting *)

  Draw.draw img menur !background None Point.zero;
  Polygon.border img menur border_size !border_color Point.zero;

  (* less: nitems_to_draw if scrolling *)
  for i = 0 to nitems_to_draw - 1 do
    paint_item img font i  entries textr false Nothing
  done;

  (* interact *)

  (* todo? have a state machine instead? and inline code of scan_items? *)

  let rec loop_while_button m acc =
    if Mouse.has_button m button
    then begin
      let acc = scan_items img font  mouse button  acc entries textr  save in
      (match acc with
      | Some _ -> acc
      | None ->
        let rec loop_while_outside_textr_and_button m =
          if not (Rectangle.pt_in_rect m.Mouse.pos textr) &&
             Mouse.has_button m button
          then
            (* less: if scrolling *)
            loop_while_outside_textr_and_button 
              (Mouse.flush_and_read display  mouse)
          else 
            (* maybe back in the textr! *)
            loop_while_button m None
        in
        (* bugfix: need to read another 'm', not pass the old 'm' above *)
        loop_while_outside_textr_and_button 
          (Mouse.flush_and_read display mouse)
      )
    end else acc
  in
  let iopt = loop_while_button (Mouse.mk pos button) (Some lasti) in

  Layer.free img;
  Display.flush display;
  (* bugfix: must run callback after menu has been erased! *)
  iopt |> Common.if_some (fun i ->
    let (_, f) = List.nth items i in
    f ()
  );
