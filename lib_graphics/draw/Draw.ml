open Common

module D = Display
module I = Display (* Image.t is in display.ml *)
module M = Draw_marshal

type op =
  | SoverD

let bp_op _op =
  failwith "bp_op: TODO"

(* less: errorfn? fontname? *)
let init _label =
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

(* even more specialized but often used *)
let draw_color dst r color =
  (* less: assert it's 1x1 *)
  draw dst r color None Point.zero

(* in draw_utils? *)
let qmask = ref None
let alloc_mix_colors display color1 color2 =
  let mask = Fun_.once qmask (fun () -> 
    Image.alloc display Rectangle.r_1x1 Channel.grey8 true
      (Color.mk2 0x3F 0x3F 0x3F)
  )
  in
  let chans = display.D.image.I.chans in
  (* less: if depth <= 8 *)
  if Channel.depth_of_channels chans <= 8 then begin
    (* todo: finalize tmp! *)
    let tmp = Image.alloc display Rectangle.r_1x1 chans false color1 in
    let img = Image.alloc display (Rectangle.r 0 0 2 2) chans true color2 in
    draw img Rectangle.r_1x1 tmp None Point.zero;
    Image.free tmp;
    img
  end else begin
    (* todo: finalize tmp! *)
    let tmp = Image.alloc display Rectangle.r_1x1 chans true color1 in
    let img = Image.alloc display Rectangle.r_1x1 chans true color2 in
    draw img img.I.r tmp (Some mask) Point.zero;
    Image.free tmp;
    img
  end
