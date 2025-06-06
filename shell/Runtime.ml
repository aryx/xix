(*s: Runtime.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

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
  (* to display a prompt or not *)
  mutable iflag: bool;
  (*x: [[Runtime.thread]] other fields *)
  (* connected on stdin by default (changed when do '. file') *)
  mutable lexbuf: Lexing.lexbuf;
  (*x: [[Runtime.thread]] other fields *)
  (* for error reporting (None when reading from stdin) *)
  (* less: file has to be mutable? could be a param of start? like chan? *)
  mutable file: Common.filename option;
  line: int ref;
  (*x: [[Runtime.thread]] other fields *)
  (* Used for switch but also assignments. *)
  mutable argv_stack: (string list) list;
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

(*s: global [[Runtime.globals]] *)
let globals: (varname, var) Hashtbl.t = 
  Hashtbl.create 101
(*e: global [[Runtime.globals]] *)

(*s: global [[Runtime.fns]] *)
let fns: (string, fn) Hashtbl.t = 
  Hashtbl.create 101
(*e: global [[Runtime.fns]] *)

(* less: argv0 *)

(*s: constant [[Runtime.runq]] *)
(* less: could have also  'let cur = { code = boostrap; pc = 1; chan = stdin }'
 * in addition to runq. Then there will be no need for cur ().
 *)
let runq = ref []
(*e: constant [[Runtime.runq]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Runtime.cur]] *)
let cur () =
  match !runq with
  | [] -> failwith "empty runq"
  | x::_xs -> x
(*e: function [[Runtime.cur]] *)

(*s: function [[Runtime.push_list]] *)
let push_list () =
  let t = cur () in
  t.argv_stack <- t.argv :: t.argv_stack;
  t.argv <- []
(*e: function [[Runtime.push_list]] *)
(*s: function [[Runtime.pop_list]] *)
let pop_list () =
  let t = cur () in
  match t.argv_stack with
  (* At the very beginning we do *=(argv) in bootstrap.
   * In that case, Assign will generate two pop_list, but for
   * the second one argv_stack becomes empty. The only thing
   * we must do is to empty argv then.
   *)
  | [] ->
     t.argv <- []
  | x::xs -> 
      t.argv_stack <- xs;
      t.argv <- x
(*e: function [[Runtime.pop_list]] *)

(*s: function [[Runtime.push_word]] *)
let push_word s =
  let t = cur () in
  t.argv <- s::t.argv
(*e: function [[Runtime.push_word]] *)
(*s: function [[Runtime.pop_word]] *)
let pop_word () =
  let t = cur () in
  match t.argv with
  | [] -> failwith "pop_word but no word!"
  | _x::xs ->
      t.argv <- xs
(*e: function [[Runtime.pop_word]] *)

(*s: function [[Runtime.push_redir]] *)
let push_redir (x : redir) : unit =
  let t = cur () in
  match t.redirections with
  | [] -> failwith "push_redir: no starting redir"
  | xs::xxs -> t.redirections <- (x::xs)::xxs
(*e: function [[Runtime.push_redir]] *)
(*s: function [[Runtime.pop_redir]] *)
let pop_redir () =
  let t= cur () in
  match t.redirections with
  | [] -> failwith "pop_redir: no starting redir"
  | []::_xxs -> failwith "popredir null!"
  | (x::xs)::xxs ->
      t.redirections <- xs::xxs;
      (match x with
      | FromTo (fd_from, _fd_to) ->
          Unix.close fd_from
      | Close _ ->
          ()
      )
(*e: function [[Runtime.pop_redir]] *)

(*s: function [[Runtime.turf_redir]] *)
let turf_redir () =
  let t = cur () in
  while List.hd t.redirections <> [] do
    pop_redir ()
  done
(*e: function [[Runtime.turf_redir]] *)

(*s: function [[Runtime.doredir]] *)
let doredir xxs =
  xxs |> List.flatten |> List.rev |> List.iter (fun redir ->
    match redir with
    | FromTo (xfrom, xto) ->
        Unix.dup2 xfrom xto;
        Unix.close xfrom
    | Close from ->
        Unix.close from
  )
(*e: function [[Runtime.doredir]] *)

(*s: function [[Runtime.mk_thread]] *)
(* This function was called start(), but it does not really start right
 * away the new thread. So better to call it mk_thread.
 * It starts with pc <> 0 when handle async, traps, pipes, etc.
 *)
let mk_thread code pc locals =
  let t = {
    code = code;
    pc = ref pc;
    argv = [];
    locals = locals;

    (*s: [[Runtime.mk_thread()]] set other fields *)
    redirections = 
      (match !runq with
      | [] -> []::[]
      | t::_ts -> []::t.redirections
      );
    (*x: [[Runtime.mk_thread()]] set other fields *)
    waitstatus = NothingToWaitfor;
    (*x: [[Runtime.mk_thread()]] set other fields *)
    iflag = false;
    (*x: [[Runtime.mk_thread()]] set other fields *)
    lexbuf = Lexing.from_function (fun _ _ -> failwith "unconnected lexbuf");
    (*x: [[Runtime.mk_thread()]] set other fields *)
    file = None;
    line = ref 1;
    (*x: [[Runtime.mk_thread()]] set other fields *)
    argv_stack = [];
    (*e: [[Runtime.mk_thread()]] set other fields *)
  } in
  t
(*e: function [[Runtime.mk_thread]] *)
(*e: Runtime.ml *)
