open Common
open Regexp_.Operators

open Point
open Rectangle

(* todo: delete once threadUnix is not needed anymore *)
module Unix1 = Unix
module Unix2 = ThreadUnix

module M = Draw_marshal

type image = {
  id: int;
  r: Rectangle.t;

  chans: Channel.t;
  (* derives from chan *)
  depth: int;

  (* mutable? *)
  clipr: Rectangle.t;
  repl: bool;

  display: display;

  (* baselayer id when image is a layer, but the information is not
   * really used client-side, so we could remove this field.
   * Still useful as a way to sanity check that an image is a layer
   * in layer.ml.
   *)
  baseid: int option;
}

and display = {
  (* the "screen" (or "view" when run inside a window) *)
  image: image;
  (* less: have also a screenimage? or baselayerimage? or chans_reference?
   * to be used in Layer.alloc
   *)

  (* /dev/draw/x *)
  dirno: int;

  (* /dev/draw/x/ctl *)
  ctl: Unix1.file_descr;
  (* /dev/draw/x/data *)
  data: Unix1.file_descr;

  (* set later in Draw.init, not Display.init *)
  mutable white: image;
  mutable black: image;
  mutable opaque: image;
  mutable transparent: image;

  mutable imageid: int;
  
  (* size = Image.bufsize + 1 (* for 'v' *)  *)
  buf: bytes;
  (* between 0 and String.length buf *)
  mutable bufp: int;

  (* less: list of layers? why need that? when free display? *)
  (* less: defaultfont? defaultsubfont? seems unused *)
}

type t = display

(* Should be in display.ml, but t and display are mutually recursive
 * which forces us to define those helpers here.
 *)

(* less: iounit(data)? *)
let bufsize = 8000 (* actually 8000+1 for 'v' when flush_display  *)

let flush_buffer display =
  let n = display.bufp in
  (* bugfix: important to do that before the exn, otherwise will
   * get into a loop and resend again and again the same
   * message (e.g., in Baselayer.alloc with the retry)
   *)
  display.bufp <- 0;
  if n <> 0
  then begin
    let n2 = 
      try 
        Unix2.write display.data display.buf 0 n 
      with Unix1.Unix_error (err, _write, s2) ->
        failwith (spf "error '%s(%s)' writing %d bytes |%s|" 
                    (Unix1.error_message err) s2
                    n (String.escaped (Bytes.sub_string display.buf 0 n)))
    in
    (* stricter: not only if drawdebug but always *)
    if n2 <> n
    then failwith (spf "wrote only %d, not %d in /dev/draw/x/data" n2 n);
  end

let add_buf display str =
  let len = String.length str in
  if len >= bufsize
  then failwith (spf "too big string (size = %d > bufsize)" len);
  
  if len + display.bufp >= bufsize
  then flush_buffer display;

  String.blit str 0 display.buf display.bufp len;
  display.bufp <- display.bufp + len


let rec fake_image = { 
   id = -1; chans = []; depth = -1; repl = false;
   r = Rectangle.r_empty; clipr =  Rectangle.r_empty; 
   display = fake_display; baseid = None;
}
and fake_display = {  
  image = fake_image; dirno = -1; ctl = Unix1.stderr; data = Unix1.stderr;
  white = fake_image; black = fake_image; 
  opaque = fake_image; transparent = fake_image;
  imageid = -1; buf = Bytes.create 0; bufp = -1;
}


(* less: devdir? windir? errorfn? *)
let init () =
  (* less: OCEXEC *)
  let ctlfd = 
    (* less: could use finalize to close if exn at least *)
    Unix1.openfile "/dev/draw/new" [Unix1.O_RDWR] 0o666 in

  let ninfo = 12 * 12 in
  let str = Bytes.make ninfo ' ' in

  let n = Unix1.read ctlfd str 0 ninfo in
  if n <> ninfo && 
     (* less: not sure why but it reads only 143 characters *)
     n <> (ninfo - 1)
  then failwith (spf "wrong format in /dev/draw/new; read %d chars (%s)" n (Bytes.to_string str));

  let str_at n = 
    let s = Bytes.sub_string str (n * 12) 12 in
    if s =~ "^[ ]*\\([^ ]+\\)[ ]*$"
    then Regexp_.matched1 s
    else failwith (spf "not a /dev/draw/new entry, got %s" s)
  in
  let int_at n = 
    try int_of_string (str_at n)
    with Failure _ -> failwith (spf "not an int at %d (%s)" n (str_at n))
  in
  let bool_at n = 
    match int_at n with 
    | 0 -> false 
    | 1 -> true 
    | x -> failwith (spf "wrong format for boolean, accept 0 or 1 not %d" x)
  in

  let clientnb = int_at 0 in

  let datafd = 
    Unix1.openfile (spf "/dev/draw/%d/data" clientnb) [Unix1.O_RDWR] 0o666 in
  let _reffdTODO =
    Unix1.openfile (spf "/dev/draw/%d/refresh" clientnb) [Unix1.O_RDONLY] 0o666 in

  let chans = Channel.channels_of_str (str_at 2) in

  let rec image = {
    id = int_at 1;
    chans = chans;
    depth = Channel.depth_of_channels chans;
    repl = bool_at 3;
    r = { min = { x = int_at 4; y = int_at 5 }; 
          max = { x = int_at 6; y = int_at 7 };
        };
    clipr = { min = { x = int_at 8; y = int_at 9 }; 
              max = { x = int_at 10; y = int_at 11 };
            };
    display = display;
    baseid = None;
  }
  and display = {
    dirno = clientnb;
    ctl = ctlfd;
    data = datafd;

    image = image;
    imageid = 0;
    (* +1 for space for 'v' when flush_display() *)
    buf = Bytes.make (bufsize + 1) ' ';  
    bufp = 0;

    (* set in Draw.init *)
    white = fake_image;
    black = fake_image;
    opaque = fake_image;
    transparent = fake_image;
  }
  in
  assert(image.id = 0);

  (* todo: allocimage here? *)
  display


let flush display =
  add_buf display "v";
  flush_buffer display


let debug_set display b =
  add_buf display ("D" ^ M.bp_bool b);
  flush_buffer display

let debug display =
  debug_set display true

let with_debug display f =
  debug_set display true;
  f ();
  debug_set display false;
  ()

