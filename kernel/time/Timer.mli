
type t = Timer_.t
type mode = Timer_.mode

val alloc : mode -> Time.t_ns -> (unit -> unit) -> t

val add : t -> t list -> t list * int option
val del : t -> t list -> t list * int option
