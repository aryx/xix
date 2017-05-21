open Common

(* todo: delete once threadUnix is not needed anymore *)
module Unix1 = Unix
module Unix2 = ThreadUnix

open Display
module M = Draw_marshal

type t = Display.image

(* less: _allocimage and initial image (and screenid and refresh) and
 *  optional color (can pass -1 mean no color to set)
 *)
let alloc display r chans repl color =
  let depth = Channel.depth_of_channels chans in
  if chans = [] || depth = 0
  then failwith "bad channel descriptor";

  (* meh *)
  flush_buffer display;

  display.imageid <- display.imageid + 1;
  let id = display.imageid in

  (* less: _allocimage with screenid, refresh *)
  let screenid = 0 in
  let refresh = false in

  let clipr = 
    if repl
    then Rectangle.r (-0x3FFFFFFF) (-0x3FFFFFFF) 0x3FFFFFFF 0x3FFFFFFF
    else r
  in

  let str = "b" ^ M.bp_long id ^ M.bp_long screenid ^ 
    M.bp_bool refresh ^ M.bp_chans chans ^ M.bp_bool repl ^
    M.bp_rect r ^ M.bp_rect clipr ^ 
    M.bp_color color
  in
  add_buf display str;

  { id = id;
    r = r;
    clipr = clipr;
    chans = chans;
    depth = depth;
    repl = repl;
    display = display;
  }


let free img =
  raise Todo

(* shortcuts *)
let alloc_color display color =
  (* less: opti: could also use view.chan as default channel as in menuhits *)
  alloc display (Rectangle.r  0 0 1 1) Channel.rgb24 true color
