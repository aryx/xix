
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
  ctl: Unix.file_descr;
  (* /dev/draw/x/data *)
  data: Unix.file_descr;

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

val bufsize : int

val fake_display: t
val fake_image: image

val init: unit -> t

val flush: t -> unit

(* ?? *)
val flush_buffer: t -> unit

(* ?? *) 
val add_buf: t -> string -> unit

(* ?? *)
val debug: t -> unit
