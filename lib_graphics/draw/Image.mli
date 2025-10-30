
type t = Display.image

val fake_image: t

type refresh =
  | RefreshNone
  | RefreshBackup

val alloc: 
  Display.t -> Rectangle.t -> Channel.t -> bool -> Color.t -> t

val alloc_color:
  Display.t -> Color.t -> t

val alloc_gen: 
  Display.t -> Rectangle.t -> Channel.t -> bool -> Color.t ->
  int option -> refresh -> t              

val free: t -> unit

val flush: t -> unit

(* ?? *)
val load: t -> Rectangle.t -> string -> int

