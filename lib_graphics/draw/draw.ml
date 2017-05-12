open Common
open Point
open Rectangle


(* less: errorfn? fontname? *)
let init label =
  (* less: sanity check /dev/draw/new exists and bind("#i", "/dev") *)
  let display = Display.init () in
  (* less: atexit(drawshutdown) *)
  (* todo: font, label, window *)
  display
