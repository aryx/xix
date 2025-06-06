(*s: Runtime.mli *)

(*s: type [[Runtime.varname]] *)
(* can be anything: "foo", but also "*", "1", "2", etc *)
type varname = string
(*e: type [[Runtime.varname]] *)
(*s: type [[Runtime.value]] *)
(* In rc the basic (and only) value is the list of strings.
 * A single word is considered as a list with one element.
 *)
type value = string list
(*e: type [[Runtime.value]] *)
(*s: type [[Runtime.var]] *)
type var = { 
  (* can be None when lookup for a value that was never set before,
   * which is different from Some [] (when do A=()), which is also
   * different from Some [""] (when do A='').
   *)
  mutable v: value option;
  (* less: opti: changed: bool *)
}
(*e: type [[Runtime.var]] *)
(*s: type [[Runtime.fn]] *)
type fn = {
  code: Opcode.codevec;
  pc: int;
  (* less: fnchanged *)
}
(*e: type [[Runtime.fn]] *)

(*s: type [[Runtime.thread]] *)
type thread = {
  code: Opcode.codevec;
  pc: int ref;

  mutable argv: string list;
  locals: (varname, var) Hashtbl.t;

  (*s: [[Runtime.thread]] other fields *)
  (* things to do before exec'ing the simple command *)
  mutable redirections: (redir list) list;
  (*x: [[Runtime.thread]] other fields *)
  (* things to wait for after a thread forked a process *)
  mutable waitstatus: waitstatus;
  (*x: [[Runtime.thread]] other fields *)
  (* Used for switch but also assignments. *)
  mutable argv_stack: (string list) list;
  (*x: [[Runtime.thread]] other fields *)
  (* for error reporting (None when reading from stdin) *)
  (* less: file has to be mutable? could be a param of start? like chan? *)
  mutable file: Common.filename option;
  line: int ref;
  (*x: [[Runtime.thread]] other fields *)
  (* connected on stdin by default (changed when do '. file') *)
  mutable lexbuf: Lexing.lexbuf;
  (*x: [[Runtime.thread]] other fields *)
  (* to display a prompt or not *)
  mutable iflag: bool;
  (*e: [[Runtime.thread]] other fields *)
}
(*e: type [[Runtime.thread]] *)
(*s: type [[Runtime.redir]] *)
  and redir =
    (* the file descriptor From becomes To, e.g., /tmp/foo becomes stdout,
     * which means your process output will now go in /tmp/foo.
     *)
    | FromTo of Unix.file_descr (* from *) * Unix.file_descr (* to *)
    | Close of Unix.file_descr
(*e: type [[Runtime.redir]] *)

(*s: type [[Runtime.waitstatus]] *)
  and waitstatus =
    | NothingToWaitfor
    (* process pid to wait for in Xpipewait (set from Xpipe) *)
    | WaitFor of int 
    (* exit status from child process returned from a wait() *)
    | ChildStatus of string
(*e: type [[Runtime.waitstatus]] *)

(* globals *)

(*s: signature [[Runtime.globals]] *)
val globals : (varname, var) Hashtbl.t
(*e: signature [[Runtime.globals]] *)
(*s: signature [[Runtime.fns]] *)
val fns : (string, fn) Hashtbl.t
(*e: signature [[Runtime.fns]] *)
(*s: signature [[Runtime.runq]] *)
val runq : thread list ref
(*e: signature [[Runtime.runq]] *)

(* API *)

(*s: signature [[Runtime.cur]] *)
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
