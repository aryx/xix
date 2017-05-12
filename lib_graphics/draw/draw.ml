open Common
open Point
open Rectangle

type op =
  | SoverD


(* less: errorfn? fontname? *)
let init label =
  (* less: sanity check /dev/draw/new exists and bind("#i", "/dev") *)
  let display = Display.init () in
  (* todo: allocimage here? *)

  (* less: atexit(drawshutdown) *)
  (* todo: font, label, window *)
  display
