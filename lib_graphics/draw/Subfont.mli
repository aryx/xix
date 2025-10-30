

type t = {
  name: string;
  bits: Image.t;

  (* /* n+1 character descriptors */? still need n+1 trick? *)
  chars: Fontchar.t array;

  (*/* max height of image, interline spacing */*)
  height : int;
  (*/* top of image to baseline */*)
  ascent: int;

  (* less: refcounter ? *)
}

val alloc: string -> int -> int -> int -> Fontchar.t array -> Image.t -> t

