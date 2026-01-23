(*s: Out.mli *)
(*s: signature [[Out.putchr]] *)
val putchr: Env.t -> char -> unit
(*e: signature [[Out.putchr]] *)
(*s: signature [[Out.putst]] *)
val putst: Env.t -> string -> unit
(*e: signature [[Out.putst]] *)
(*s: signature [[Out.putshst]] *)
(* ed: put "shell" string, legacy name, identical to putst for oed *)
val putshst : Env.t -> string -> unit 
(*e: signature [[Out.putshst]] *)

(*s: signature [[Out.putd]] *)
(* will print Env.count *)
val putd: Env.t -> unit
(*e: signature [[Out.putd]] *)
(*e: Out.mli *)
