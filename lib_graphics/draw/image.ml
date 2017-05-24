open Common
open Point
open Rectangle

(* todo: delete once threadUnix is not needed anymore *)
module Unix1 = Unix
module Unix2 = ThreadUnix

open Display
module M = Draw_marshal

type t = Display.image

type refresh =
  | RefreshNone
  | RefreshBackup
  (* less: RefreshMesg *)

(* less: _allocimage and initial image  and
 *  optional color (can pass -1 mean no color to set)
 *)
let alloc_gen display r chans repl color baseopt refopt =
  let depth = Channel.depth_of_channels chans in
  if chans = [] || depth = 0
  then failwith "bad channel descriptor";

  (* meh *)
  flush_buffer display;

  display.imageid <- display.imageid + 1;
  let id = display.imageid in

  (* less: _allocimage with screenid, refresh *)
  let screenid = 
    match baseopt with
    | None -> 0
    | Some x -> x
  in
  let refresh = 
    match refopt with
    | RefreshNone -> 1
    | RefreshBackup -> 0
  in

  let clipr = 
    if repl
    then Rectangle.r (-0x3FFFFFFF) (-0x3FFFFFFF) 0x3FFFFFFF 0x3FFFFFFF
    else r
  in

  let str = "b" ^ M.bp_long id ^ M.bp_long screenid ^ 
    M.bp_byte refresh ^ M.bp_chans chans ^ M.bp_bool repl ^
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
    baseid = baseopt;
  }

 
(* less: color opt *)
let alloc display r chans repl color =
  alloc_gen display r chans repl color None RefreshNone


let free img =
  let display = img.display in

  (* meh *)
  flush_buffer display;

  let str = "f" ^ M.bp_long img.id in
  (* less: should flush display only if img is a layer, but
   * for now I just always flush
   *)
  add_buf display str;
  Display.flush display;
  ()


(* shortcuts *)
let alloc_color display color =
  (* less: opti: could also use view.chan as default channel as in menuhits *)
  alloc display (Rectangle.r  0 0 1 1) Channel.rgb24 true color

let flush img =
  Display.flush img.display

(* less: move in channel.ml? *)
let bytes_per_line r depth =
  match depth with
  | 1 -> Rectangle.dx r / 8
  | _ -> raise Todo

let load img r bytes =
  let display = img.display in

  if not (Rectangle.rect_in_rect r img.r)
  then failwith "Image.load: bad rectangle";

  (* note that Unix.write does already split in chunk of 16384 bytes *)
  let chunk = Display.bufsize - 64 (* for 'y' id[4] ... header *) in

  let bpl = bytes_per_line r img.depth in
  let n = bpl * Rectangle.dy r in
  if n > String.length bytes
  then failwith "Image.load: insufficient data";

  let r = ref r in
  let ndata = ref 0 in
  while !r.min.y < !r.max.y do
    let dy = !r.max.y - !r.min.y in
    let dy = 
      if dy * bpl > chunk
      then chunk / bpl
      else dy
    in
    if dy <= 0
    then failwith "Image.load: image too wide for buffer";
    let n = dy * bpl in

    let str = "y" ^ M.bp_long img.id ^ M.bp_rect 
      { !r with max = { !r.max with y = !r.min.y + dy } } ^
      (String.sub bytes !ndata n)
    in
    add_buf display str;
    r := { !r with min = { !r.min with y = !r.min.y + dy } };
    ndata := !ndata + n;
  done;
  !ndata

