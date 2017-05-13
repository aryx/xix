open Common

module M = Draw_marshal

type t = {
  id: int;
  r: Rectangle.t;

  chans: Channel.t;
  (* derives from chan *)
  depth: int;

  (* mutable? *)
  clipr: Rectangle.t;
  repl: bool;

  display: display;  
}

and display = {
  (* the "screen" (or "view" when run inside a window) *)
  image: t;

  (* /dev/draw/x *)
  dirno: int;

  (* /dev/draw/x/ctl *)
  ctl: Unix.file_descr;
  (* /dev/draw/x/data *)
  data: Unix.file_descr;

(*
  white: t;
  black: t;
  opaque: t;
  transparent: t;
*)
  mutable imageid: int;
  
  (* size = Image.bufsize + 1 (* for 'v' *)  *)
  buf: string;
  (* between 0 and String.length buf *)
  mutable bufp: int;
}

(* less: iounit(data)? *)
let bufsize = 8000 (* actually 8000+1 for 'v' when flush_display  *)

let flush_buffer display =
  let n = display.bufp in
  if n <> 0
  then begin
    let n2 = Unix.write display.data display.buf 0 n in
    (* less: only if drawdebug? *)
    if n2 <> n
    then failwith (spf "wrote only %d, not %d in /dev/draw/x/data" n2 n);
    display.bufp <- 0;
  end

let add_buf display str =
  let len = String.length str in
  if len >= bufsize
  then failwith (spf "too big string (size = %d > bufsize)" len);
  
  if len + display.bufp >= bufsize
  then flush_buffer display;

  String.blit str 0 display.buf display.bufp len;
  display.bufp <- display.bufp + len

let flush_display display =
  add_buf display "v";
  flush_buffer display


(* less: _allocimage and initial image (and screenid and refresh) *)
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

  let str = "b" ^ M.bp_long id ^ M.bp_long screenid ^ M.bp_bool refresh ^
    M.bp_rect r ^ M.bp_rect clipr ^ M.bp_color color
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
