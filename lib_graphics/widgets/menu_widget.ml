open Common

module I = Image

(* inspiration is GToolbox in lablgtk *)
type item = (string * (unit -> unit))
type items = item list

let vspacing   = 2 (* extra spacing between lines of text *)
let margin     = 4 (* outside to text *)
let border_size = 2 (* width of outlining border *)

let background      = ref Display.fake_image
let high            = ref Display.fake_image
let border          = ref Display.fake_image
let text            = ref Display.fake_image
let highligted_text = ref Display.fake_image
let menutext        = ref Display.fake_image

let init_colors display =
  if !background == Display.fake_image
  then begin
    (* less: could use try and default to black/white if can not alloc image*)
    (* less: opti: use view->chan instead of rgb24 used by Image.alloc_color *)
    (* todo: allocimagemix? *)
    background := Image.alloc_color display (Color.palegreen);
    high := Image.alloc_color display (Color.darkgreen);
    border := Image.alloc_color display (Color.medgreen);
    text := display.I.black;
    highligted_text := !background;
  end

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

  Draw.draw img menur !background None Point.zero;
  Polygon.border img menur border_size !border Point.zero;

  (* todo: Layer.free *)
  Display.flush display
    
