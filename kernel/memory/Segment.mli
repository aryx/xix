
type t = Segment_.t
type kind = Segment_.kind

val alloc: Segment_.kind -> Types.user_addr -> int -> t
val free: t -> unit
val copy: t -> t
val share: t -> t
