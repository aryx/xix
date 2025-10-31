
type display = {
  (* the "screen" (or "view" when run inside a window) *)
  image: image;

  (* /dev/draw/x *)
  dirno: int;
  (* /dev/draw/x/ctl *)
  ctl: Unix.file_descr;
  (* /dev/draw/x/data *)
  data: Unix.file_descr;

  mutable white: image;
  mutable black: image;
  mutable opaque: image;
  mutable transparent: image;

  mutable imageid: int;
  
  buf: bytes;
  mutable bufp: int;
}

and image = {
  id: int;
  r: Rectangle.t;

  chans: Channel.t;
  (* derives from chan *)
  depth: int;

  clipr: Rectangle.t;
  repl: bool;

  display: display;

  baseid: int option;
}

type t = display

val init: <Cap.draw; .. > -> t


val bufsize : int

val fake_display: t
val fake_image: image

val flush: t -> unit

(* ?? *)
val flush_buffer: t -> unit

(* ?? *) 
val add_buf: t -> string -> unit

(* Logs *)
val debug: t -> unit
