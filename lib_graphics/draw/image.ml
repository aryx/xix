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
}
