
type t = {
  (* e.g., /lib/font/bit/lucm/latin1.9.font *)
  name: string (* TODO: Fpath.t*);

  (*/* max height of image, interline spacing */*)
  height : int;
  (*/* top of image to baseline */*)
  ascent: int;

  (* less: display?*)

  subfont_spec: subfont_spec list;

  cache_img: Image.t;

  (* TODO: simplified font handling: just one subfont *)
  subfont: Subfont.t option;
}
(* old: was called Cachefont in draw-C *)
and subfont_spec = {
  min_rune: Rune.t;
  max_rune: Rune.t;

  (*/* position in subfont of character at min */*)
  offset: int option;
  (*/* stored in font*/ relative filename *)
  subfont_name: string;
  (* absolute filename computed by subfontname *)
  subfont_filename: string (*TODO: Fpath.t *);
}

val initialize: Image.t -> int -> int -> unit

val load_char: Image.t -> Image.t -> int -> Fontchar.t -> int -> unit

