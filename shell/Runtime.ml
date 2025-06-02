(*s: Runtime.ml *)
open Stdcompat (* for |> *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

(* can be anything: "foo", but also "*", "1", "2", etc *)
(*s: type [[Runtime.varname]] *)
type varname = string
(*e: type [[Runtime.varname]] *)

(* In rc the basic (and only) value is the list of strings.
 * A single word is considered as a list with one element.
 *)
(*s: type [[Runtime.value]] *)
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

  (* Used for switch but also assignments. *)
  mutable argv_stack: (string list) list;

  (* connected on stdin by default (changed when do '. file') *)
  mutable lexbuf: Lexing.lexbuf;
  (* to display a prompt or not *)
  mutable iflag: bool;

  (* for error reporting (None when reading from stdin) *)
  (* less: file has to be mutable? could be a param of start? like chan? *)
  mutable file: Common.filename option;
  line: int ref;

  (* things to do before exec'ing the simple command *)
  mutable redirections: (redir list) list;

  (* things to wait for after a thread forked a process *)
  mutable waitstatus: waitstatus;
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

let (globals: (varname, var) Hashtbl.t) = 
  Hashtbl.create 101

let (fns: (string, fn) Hashtbl.t) = 
  Hashtbl.create 101

(* less: argv0 *)

(* less: could have also  'let cur = { code = boostrap; pc = 1; chan = stdin }'
 * in addition to runq. Then there will be no need for cur ().
 *)
(*s: constant [[Runtime.runq]] *)
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
let push_redir x =
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
(*e: function [[Runtime.pop_redir]] *)
      )

(*s: function [[Runtime.turf_redir]] *)
let turf_redir () =
  let t = cur () in
  while List.hd t.redirections <> [] do
    pop_redir ()
(*e: function [[Runtime.turf_redir]] *)
  done
  

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
    argv_stack = [];
    locals = locals;

    lexbuf = Lexing.from_function (fun _ _ -> failwith "unconnected lexbuf");
    iflag = false;
    file = None;
    line = ref 1;

    waitstatus = NothingToWaitfor;
    redirections = 
      (match !runq with
      | [] -> []::[]
      | t::_ts -> []::t.redirections
      );
  } in
  t
(*e: function [[Runtime.mk_thread]] *)
  (* old: do that in caller now, so more explicit 
  runq := t::!runq
  *)
(*e: Runtime.ml *)
