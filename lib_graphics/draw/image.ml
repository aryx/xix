open Common

(* todo: delete once threadUnix is not needed anymore *)
module Unix1 = Unix
module Unix2 = ThreadUnix

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
  ctl: Unix1.file_descr;
  (* /dev/draw/x/data *)
  data: Unix1.file_descr;

  (* set later in Draw.init, not Display.init *)
  mutable white: t;
  mutable black: t;
  mutable opaque: t;
  mutable transparent: t;

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
    let n2 = 
      try 
        Unix2.write display.data display.buf 0 n 
      with Unix1.Unix_error (err, _write, s2) ->
        failwith (spf "error '%s(%s)' writing %d bytes |%s|" 
                    (Unix1.error_message err) s2
                    n (String.escaped (String.sub display.buf 0 n)))
    in
    (* stricter: not only if drawdebug but always *)
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
  alloc display (Rectangle.r  0 0 1 1) Channel.rgb24 true color
