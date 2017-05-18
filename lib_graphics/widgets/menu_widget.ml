open Common

module I = Image

(* inspiration is GToolbox in lablgtk *)
type item = (string * (unit -> unit))
type items = item list

let back = ref Display.fake_image
let high = ref Display.fake_image
let border = ref Display.fake_image
let text = ref Display.fake_image
let highligted_text = ref Display.fake_image
let menutext = ref Display.fake_image

let init_colors display =
  if !back == Display.fake_image
  then begin
    (* less: could use try and default to black/white if can not alloc image*)
    (* less: opti: use view->chan instead of rgb24 used by Image.alloc_color *)
    (* todo: allocimagemix? *)
    back := Image.alloc_color display (Color.palegreen);
    high := Image.alloc_color display (Color.darkgreen);
    border := Image.alloc_color display (Color.medgreen);
    text := display.I.black;
    highligted_text := !back;
  end

let menu items button (m, mouse) (display, desktop, view, font) =
  init_colors display;
  (* less: reset clipr and repl on view? *)
  let max_width = 
    items |> List.map (fun (str, _f) -> Font.string_width font str)
          |> List.fold_left max 0
  in
  (* pr (spf "size = %d" max_width) *)
  raise Todo
