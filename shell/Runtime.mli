
type varname = string
type value = string list
type var = { mutable v : value option; }
type fn = { code : Opcode.codevec; pc : int; }

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
and redir =
    FromTo of Unix.file_descr * Unix.file_descr
  | Close of Unix.file_descr

and waitstatus = NothingToWaitfor | WaitFor of int | ChildStatus of string

(* globals *)
val globals : (varname, var) Hashtbl.t
val fns : (string, fn) Hashtbl.t
val runq : thread list ref

(* API *)

val cur : unit -> thread
val push_list : unit -> unit
val pop_list : unit -> unit
val push_word : string -> unit
val pop_word : unit -> unit
val push_redir : redir -> unit
val pop_redir : unit -> unit
val turf_redir : unit -> unit
val doredir : redir list list -> unit

val mk_thread :
  Opcode.codevec -> int -> (varname, var) Hashtbl.t -> thread
