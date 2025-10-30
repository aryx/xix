open Common

(*
/*
 * Subfonts
 *
 * given char c, Subfont *f, Fontchar *i, and Point p, one says
 *	i = f->info + c;
 *	draw(b, Rect(p.x + i->left, p.y + i->top,
 *		p.x + i->left + ((i+1)->x - i->x), p.y + i->bottom),
 *		color, f->bits, Pt(i->x, i->top));
 *	p.x += i->width;
 * to draw characters in the specified color (itself an Image) in Image b.
 */
*)
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

(* todo: h_subfonts *)

let alloc name nfontchars height ascent fontchars img =
  assert (Array.length fontchars = nfontchars + 1);
  assert (height <> 0);
  let sf = 
    { name = name;
      bits = img;
      chars = fontchars;
      height = height;
      ascent = ascent;
    }
  in
  (* less: install subfont if name? *)
  sf
