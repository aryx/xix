open Common
open Point
open Rectangle

module I = Image

(* inspiration is GToolbox in lablgtk *)
type item = (string * (unit -> unit))
type items = item list

let vspacing   = 2 (* extra spacing between lines of text *)
let margin     = 4 (* outside to text *)
let border_size = 2 (* width of outlining border *)

let background             = ref Display.fake_image
let background_highlighted = ref Display.fake_image
let border_color           = ref Display.fake_image
let text_color             = ref Display.fake_image
let text_highlighted       = ref Display.fake_image
(*let menutext        = ref Display.fake_image *)

(* todo: remove once get List.iteri in 1.07 *)
let list_iteri f xs =
  xs |> Array.of_list |> Array.iteri f

let init_colors display =
  if !background == Display.fake_image
  then begin
    (* less: could use try and default to black/white if can not alloc image*)
    (* less: opti: use view->chan instead of rgb24 used by Image.alloc_color *)
    (* todo: allocimagemix? *)
    background := Image.alloc_color display (Color.palegreen);
    background_highlighted := Image.alloc_color display (Color.darkgreen);
    border_color := Image.alloc_color display (Color.medgreen);
    text_color := display.I.black;
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
  Rectangle.insetrect r (border_size - margin)


let paint_item img str i textr font highlight =
  (* todo: save and restore *)
  let line_height = font.Font.height + vspacing in
  let r = rect_of_menu_entry textr i font in
  let pt = { x = (textr.min.x + textr.max.x - Font.string_width font str) / 2;
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

let scan_items img mouse textr save =
  raise Todo


let menu items button (m, mouse) (display, desktop, view, font) =
  init_colors display;
  (* less: reset clipr and repl on view? *)
  let max_width = 
    items |> List.map (fun (str, _f) -> Font.string_width font str)
          |> List.fold_left max 0
  in
  (* pr (spf "size = %d" max_width) *)
  (* todo: if scrolling *)
  let width = max_width in
  let nitems_to_draw = List.length items in
  (* less: save lasti *)
  let lasti = 0 in

  let line_height = font.Font.height + vspacing in
  let height = nitems_to_draw * line_height in

  let r = Rectangle.r 0 0 width height in
  let r = Rectangle.insetrect r (-margin) in
  (* center on last entry *)
  let r = Rectangle.sub_pt r 
    (Point.p (width / 2) (lasti * line_height + font.Font.height / 2)) in
  (* adjust to mouse position *)
  let r = Rectangle.add_pt r m.Mouse.pos in
  
  (* less: adjust if outside view *)
  let pt_adjust = Point.zero in
  let menur = Rectangle.add_pt r pt_adjust in

  let textr = Rectangle.insetrect menur margin in

  (* todo: Layer.alloc *)
  (* less: handle case where no desktop? *)
  let img = view in
  (* todo: save *)
  let save = () in

  Draw.draw img menur !background None Point.zero;
  Polygon.border img menur border_size !border_color Point.zero;

  (* less: nitems_to_draw if scrolling *)
  items |> list_iteri (fun i (str, _f) ->
    paint_item img str i textr font false
  );

  let rec loop_while_button m acc =
    if Mouse.has_button m button
    then begin
      let lasti_opt = scan_items img mouse textr save in
      (match lasti_opt with
      | Some x -> Some x
      | None ->
        let rec loop_while_outside_textr_and_button m acc =
          if not (Rectangle.pt_in_rect m.Mouse.pos textr) &&
             Mouse.has_button m button
          then begin
            (* less: if scrolling *)
            loop_while_outside_textr_and_button 
              (Mouse.receive mouse |> Event.sync) acc
          end
          else loop_while_button m acc
        in
        loop_while_outside_textr_and_button m acc
      )
    end else acc
  in
  let lasti_opt = loop_while_button m (Some lasti) in

  (* todo: Layer.free *)
  Display.flush display
    
