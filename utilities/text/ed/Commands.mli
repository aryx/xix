(*s: Commands.mli *)

(*s: signature [[Commands.read]] *)
(* 'r' *)
val read: <Cap.open_in; ..> -> Env.t -> Fpath.t -> unit
(*e: signature [[Commands.read]] *)
(*s: signature [[Commands.write]] *)
(* 'w'*)
val write: <Cap.open_out; ..> -> Env.t -> Fpath.t -> unit
(*e: signature [[Commands.write]] *)
(*s: signature [[Commands.add]] *)
(* 'a' and 'i' *)
val add: Env.t -> int -> unit
(*e: signature [[Commands.add]] *)
(*s: signature [[Commands.printcom]] *)
(* 'p' *)
val printcom : Env.t -> unit
(*e: signature [[Commands.printcom]] *)
(*s: signature [[Commands.rdelete]] *)
(* 'd' and 'c' *)
val rdelete: Env.t -> Env.lineno -> Env.lineno -> unit
(*e: signature [[Commands.rdelete]] *)
(*s: signature [[Commands.quit]] *)
(* 'q' (need open_out to remove Env.tfname from the filesystem) *)
val quit: < Cap.open_out; ..> -> Env.t -> unit
(*e: signature [[Commands.quit]] *)
(* 's' *)
val substitute: Env.t -> bool (* inglob *) -> unit
(* '!' *)
val callunix: <Cap.forkew; ..> -> Env.t -> unit

(*s: signature [[Commands.setwide]] *)
(* helpers *)
val setwide: Env.t -> unit
(*e: signature [[Commands.setwide]] *)
(*s: signature [[Commands.squeeze]] *)
val squeeze: Env.t -> int -> unit
(*e: signature [[Commands.squeeze]] *)
(*s: signature [[Commands.nonzero]] *)
val nonzero: Env.t -> unit
(*e: signature [[Commands.nonzero]] *)
(*s: signature [[Commands.setnoaddr]] *)
val setnoaddr: Env.t -> unit
(*e: signature [[Commands.setnoaddr]] *)
val match_: Env.t -> Regex.t -> Env.lineno -> bool

(*s: signature [[Commands.append]] *)
(* return number of lines added, but usually ignored by caller *)
val append: Env.t -> (unit -> string option) -> Env.lineno -> int
(*e: signature [[Commands.append]] *)
(*e: Commands.mli *)
