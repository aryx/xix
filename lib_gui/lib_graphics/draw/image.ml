open Common

(* Plan 9 has a uniform image type used to represent everything: the screen,
 * images, but also colors which are just 1x1 images. This allows the
 * draw() function to be very generic.
 * In OCaml we cheat and allows only a subset of draw() operations.
 *)
type t = {
  r: Rectangle.t;
  kind: image_kind;
}
and image_kind = 
  | Color of Color.t
  | Screen

let alloc _display r chan repl color =
  if r <> Rectangle.r_1x1 ||
    chan <> Channel.rgb24 ||
    repl <> true
  then failwith "Image.alloc: operation not supported yet";
  { r = r; kind = Color color }
