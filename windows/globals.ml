
let (windows: (Window.wid, Window.t) Hashtbl.t) = Hashtbl.create 11
let (hidden: (Window.wid, Window.t) Hashtbl.t) = Hashtbl.create 11

(* the man page of rio (rio(1)) uses the term 'current' *)
let (current: Window.t option ref)  = ref None

(* a bit like cpu(), up() in the kernel, a convenient global *)
let win () =
  !current

let debug = ref false

(* less: could be in global, or could pass it around so more functional
 * mousectl
 * kbdctl
*)

(* less: could be in global, or could pass it explicitely so more functional
let display = ref Display.fake_display
let view = ref Display.fake_image
let font = ref Font.fake_font
let desktop = ref Baselayer.fake_baselayer
*)

(* this is just too annoying to pass around *)
let red = ref Display.fake_image
let title_color = ref Display.fake_image
let title_color_light = ref Display.fake_image
