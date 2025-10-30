
type t = {
  id: int;
  (* those images are stored server-side and never used client side 
   * so we don't need to keep a reference on them here
   * base: Image.t;
   * fill: Image.t;
   *)
  display: Display.t;
}

val alloc: Display.image -> Display.image -> t

val free: t -> unit

