
val putchr: Env.t -> char -> unit

val putst: Env.t -> string -> unit

(* ed: put "shell" string, legacy name, identical to putst for oed *)
val putshst : Env.t -> string -> unit 


(* will print Env.count *)
val putd: Env.t -> unit
