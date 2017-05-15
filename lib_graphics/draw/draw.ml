open Common

module D = Image (* display type is actually in image.ml *)
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
  (* todo: font, label, window *)
  display

let adjust_str_for_op str op =
  if op = SoverD
  then str
  else "O" ^ bp_op op ^ str
  

let draw_gen dst r src p0 mask p1 op =
  (* todo: set_drawop *)
  let str = "d" ^ M.bp_long dst.Image.id ^ M.bp_long src.Image.id ^
    M.bp_long mask.Image.id ^
    M.bp_rect r ^ M.bp_point p0 ^ M.bp_point p1
  in
  Image.add_buf dst.Image.display (adjust_str_for_op str op)

let draw dst r src mask_opt p =
  (* less: have src_opt? *)
  draw_gen dst r src p 
    (match mask_opt with
    | Some x -> x
    | None -> dst.Image.display.Image.opaque
    )
    p SoverD
