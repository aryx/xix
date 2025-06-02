(*s: Runtime.mli *)

(*s: type [[Runtime.varname (Runtime.mli)]] *)
type varname = string
(*e: type [[Runtime.varname (Runtime.mli)]] *)
(*s: type [[Runtime.value (Runtime.mli)]] *)
type value = string list
(*e: type [[Runtime.value (Runtime.mli)]] *)
(*s: type [[Runtime.var (Runtime.mli)]] *)
type var = { mutable v : value option; }
(*e: type [[Runtime.var (Runtime.mli)]] *)
(*s: type [[Runtime.fn (Runtime.mli)]] *)
type fn = { code : Opcode.codevec; pc : int; }
(*e: type [[Runtime.fn (Runtime.mli)]] *)

(*s: type [[Runtime.thread (Runtime.mli)]] *)
type thread = {
  code : Opcode.codevec;
  pc : int ref;
  mutable argv : string list;
  locals : (varname, var) Hashtbl.t;
  mutable argv_stack : string list list;
  mutable lexbuf : Lexing.lexbuf;
  mutable iflag : bool;
  mutable file : Common.filename option;
  line : int ref;
  mutable redirections : redir list list;
  mutable waitstatus : waitstatus;
}
(*e: type [[Runtime.thread (Runtime.mli)]] *)
(*s: type [[Runtime.redir (Runtime.mli)]] *)
and redir =
    FromTo of Unix.file_descr * Unix.file_descr
  | Close of Unix.file_descr
(*e: type [[Runtime.redir (Runtime.mli)]] *)

(*s: type [[Runtime.waitstatus (Runtime.mli)]] *)
and waitstatus = NothingToWaitfor | WaitFor of int | ChildStatus of string
(*e: type [[Runtime.waitstatus (Runtime.mli)]] *)

(*s: signature [[Runtime.globals]] *)
(* globals *)
val globals : (varname, var) Hashtbl.t
(*e: signature [[Runtime.globals]] *)
(*s: signature [[Runtime.fns]] *)
val fns : (string, fn) Hashtbl.t
(*e: signature [[Runtime.fns]] *)
(*s: signature [[Runtime.runq]] *)
val runq : thread list ref
(*e: signature [[Runtime.runq]] *)

(*s: signature [[Runtime.cur]] *)
(* API *)

val cur : unit -> thread
(*e: signature [[Runtime.cur]] *)
(*s: signature [[Runtime.push_list]] *)
val push_list : unit -> unit
(*e: signature [[Runtime.push_list]] *)
(*s: signature [[Runtime.pop_list]] *)
val pop_list : unit -> unit
(*e: signature [[Runtime.pop_list]] *)
(*s: signature [[Runtime.push_word]] *)
val push_word : string -> unit
(*e: signature [[Runtime.push_word]] *)
(*s: signature [[Runtime.pop_word]] *)
val pop_word : unit -> unit
(*e: signature [[Runtime.pop_word]] *)
(*s: signature [[Runtime.push_redir]] *)
val push_redir : redir -> unit
(*e: signature [[Runtime.push_redir]] *)
(*s: signature [[Runtime.pop_redir]] *)
val pop_redir : unit -> unit
(*e: signature [[Runtime.pop_redir]] *)
(*s: signature [[Runtime.turf_redir]] *)
val turf_redir : unit -> unit
(*e: signature [[Runtime.turf_redir]] *)
(*s: signature [[Runtime.doredir]] *)
val doredir : redir list list -> unit
(*e: signature [[Runtime.doredir]] *)

(*s: signature [[Runtime.mk_thread]] *)
val mk_thread :
  Opcode.codevec -> int -> (varname, var) Hashtbl.t -> thread
(*e: signature [[Runtime.mk_thread]] *)
(*e: Runtime.mli *)
