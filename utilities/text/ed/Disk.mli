
val putline : Env.t -> string -> Env.tfile_offset
val getline: Env.t -> Env.lineno -> string

val getfile: Env.t -> Chan.i -> (unit -> string option)
val putfile: Env.t -> Chan.o -> unit

