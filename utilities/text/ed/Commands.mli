
(* 'r' *)
val read: <Cap.open_in; ..> -> Env.t -> Fpath.t -> unit
(* 'w'*)
val write: <Cap.open_out; ..> -> Env.t -> Fpath.t -> unit
(* 'a' and 'i' *)
val add: Env.t -> int -> unit
(* 'p' *)
val printcom : Env.t -> unit
(* 'd' and 'c' *)
val rdelete: Env.t -> Env.lineno -> Env.lineno -> unit
(* 'q' (need open_out to remove Env.tfname from the filesystem) *)
val quit: < Cap.open_out; ..> -> Env.t -> unit

(* helpers *)
val setwide: Env.t -> unit
val squeeze: Env.t -> int -> unit
val nonzero: Env.t -> unit
val setnoaddr: Env.t -> unit

(* return nubber of lines added, but usually ignored by caller *)
val append: Env.t -> (unit -> string option) -> Env.lineno -> int
