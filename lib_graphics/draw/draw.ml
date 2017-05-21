open Common

module D = Display
module I = Display (* Image.t is in display.ml *)
module M = Draw_marshal

type op =
  | SoverD

let bp_op op =
  raise Todo

(* less: errorfn? fontname? *)
let init label =
  (* less: sanity check /dev/draw/new exists and bind("#i", "/dev") *)
  let display = Display.init () in
  (* I prefer to do the allocimage here *)
  display.D.white <- 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.grey1 true Color.white;
  display.D.black <- 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.grey1 true Color.black;
  display.D.opaque <- display.D.white;
  display.D.transparent <- display.D.black;
  
  (* less: atexit(drawshutdown) *)
  (* less: font, label, window?
   *  or force caller to call separate functions for those?
   *)
  display

let adjust_str_for_op str op =
  if op = SoverD
  then str
  else "O" ^ bp_op op ^ str
  

let draw_gen dst r src p0 mask p1 op =
  (* todo: set_drawop *)
  let str = "d" ^ M.bp_long dst.I.id ^ M.bp_long src.I.id ^
    M.bp_long mask.I.id ^
    M.bp_rect r ^ M.bp_point p0 ^ M.bp_point p1
  in
  Display.add_buf dst.I.display (adjust_str_for_op str op)

let draw dst r src mask_opt p =
  (* less: have src_opt? *)
  draw_gen dst r src p 
    (match mask_opt with
    | Some x -> x
    | None -> dst.I.display.D.opaque
    )
    p SoverD
