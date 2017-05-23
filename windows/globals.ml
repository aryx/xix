
let (windows: (Window.wid, Window.t) Hashtbl.t) = Hashtbl.create 11

(* rio(1) uses the term 'current' *)
let (current: Window.t option ref)  = ref None

(* a bit like cpu(), up() in the kernel, a convenient global *)
let win () =
  !current

let debug = ref false

(* less: 
 * mousectl
 * kbdctl
*)

(*
let display = ref Display.fake_display
let view = ref Display.fake_image
let font = ref Font.fake_font
let desktop = ref Baselayer.fake_baselayer

*)
let red = ref Display.fake_image

let title_color = ref Display.fake_image
let title_color_light = ref Display.fake_image
