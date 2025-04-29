open Stdcompat (* for |> *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

(* can be anything: "foo", but also "*", "1", "2", etc *)
type varname = string

(* In rc the basic (and only) value is the list of strings.
 * A single word is considered as a list with one element.
 *)
type value = string list

type var = { 
  (* can be None when lookup for a value that was never set before,
   * which is different from Some [] (when do A=()), which is also
   * different from Some [""] (when do A='').
   *)
  mutable v: value option;
  (* less: opti: changed: bool *)
}

type fn = {
  code: Opcode.codevec;
  pc: int;
  (* less: fnchanged *)
}


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

  and redir =
    (* the file descriptor From becomes To, e.g., /tmp/foo becomes stdout,
     * which means your process output will now go in /tmp/foo.
     *)
    | FromTo of Unix.file_descr (* from *) * Unix.file_descr (* to *)
    | Close of Unix.file_descr

  and waitstatus =
    | NothingToWaitfor
    (* process pid to wait for in Xpipewait (set from Xpipe) *)
    | WaitFor of int 
    (* exit status from child process returned from a wait() *)
    | ChildStatus of string

let (globals: (varname, var) Hashtbl.t) = 
  Hashtbl.create 101

let (fns: (string, fn) Hashtbl.t) = 
  Hashtbl.create 101

(* less: argv0 *)

(* less: could have also  'let cur = { code = boostrap; pc = 1; chan = stdin }'
 * in addition to runq. Then there will be no need for cur ().
 *)
let runq = ref []

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let cur () =
  match !runq with
  | [] -> failwith "empty runq"
  | x::xs -> x

let push_list () =
  let t = cur () in
  t.argv_stack <- t.argv :: t.argv_stack;
  t.argv <- []

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


let push_word s =
  let t = cur () in
  t.argv <- s::t.argv

let pop_word () =
  let t = cur () in
  match t.argv with
  | [] -> failwith "pop_word but no word!"
  | _x::xs ->
      t.argv <- xs


let push_redir x =
  let t = cur () in
  match t.redirections with
  | [] -> failwith "push_redir: no starting redir"
  | xs::xxs -> t.redirections <- (x::xs)::xxs


let pop_redir () =
  let t= cur () in
  match t.redirections with
  | [] -> failwith "pop_redir: no starting redir"
  | []::xxs -> failwith "popredir null!"
  | (x::xs)::xxs ->
      t.redirections <- xs::xxs;
      (match x with
      | FromTo (fd_from, fd_to) ->
          Unix.close fd_from
      | Close _ ->
          ()
      )

let turf_redir () =
  let t = cur () in
  while List.hd t.redirections <> [] do
    pop_redir ()
  done
  

let doredir xxs =
  xxs |> List.flatten |> List.rev |> List.iter (fun redir ->
    match redir with
    | FromTo (xfrom, xto) ->
        Unix.dup2 xfrom xto;
        Unix.close xfrom
    | Close from ->
        Unix.close from
  )


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
      | t::ts -> []::t.redirections
      );
  } in
  t
  (* old: do that in caller now, so more explicit 
  runq := t::!runq
  *)
