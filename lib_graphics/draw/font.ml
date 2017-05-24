open Common

type t = {
  (* e.g., /lib/font/bit/lucm/latin1.9.font *)
  name: Common.filename;

  (*/* max height of image, interline spacing */*)
  height : int;
  (*/* top of image to baseline */*)
  ascent: int;

  (* less: display?*)
  subfont_spec: subfont_spec list;

  cache_img: Image.t;
}
(* old: was called Cachefont in draw-C *)
and subfont_spec = {
  min: Rune.t;
  max: Rune.t;

  (*/* position in subfont of character at min */*)
  offset: int option;
  (*/* stored in font*/ relative filename *)
  subfont_name: string;
  (* absolute filename computed by subfontname *)
  subfont_filename: Common.filename;
}

let fake_font = 
  { height = -1; name = ""; ascent = 0; subfont_spec = []; 
    cache_img = Display.fake_image 
  }

(* for default_font see font_default.ml *)
