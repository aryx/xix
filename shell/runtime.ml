open Common

type varname = string

(* In RC the basic (and only) value is the list of strings.
 * A single word is considered as a list with one element.
 *)
type value = string list

type var = { 
  mutable v: value option;
  (* less: opti: changed: bool *)
}


type thread = {
  code: Opcode.codevec;
  pc: int ref;

  mutable argv: string list;
  mutable argv_stack: (string list) list;
  locals: (varname, var) Hashtbl.t;

  (* stdin by default (can be changed when do '. file'?? when eval) *)
  mutable chan: in_channel;
  (* to display a prompt or not *)
  mutable iflag: bool;

  (* for error reporting (None when reading from stdin) *)
  (* less: file has to be mutable? could be a param of start? like chan? *)
  mutable file: Common.filename option;
  line: int ref;

  (* todo: mutable redir: 
  *)

}

let (globals: (varname, var) Hashtbl.t) = 
  Hashtbl.create 101

(* less: could have also  
 *  let run = { code = boostrap; pc = 1; chan = stdin }
 * in addition to runq. Then no need for cur ().
 *)
let runq = ref []

(* less: argv0 *)

let cur () =
  match !runq with
  | [] -> failwith "empty runq"
  | x::xs -> x

(*
let push_list () =
  let t = cur () in
  t.argv_stack <- t.argv :: t.argv_stack;
  t.argv <- []

*)

let push_word s =
  let t = cur () in
  t.argv <- s::t.argv


(* todo: starts with pc <> 0 sometimes? *)
let start code pc locals =
  let t = {
    code = code;
    pc = ref pc;
    argv = [];
    argv_stack = [];
    locals = locals;
    (* todo: cmdfile, cmdfd, eof, iflag *)
    chan = stdin;
    iflag = false;
    file = None;
    line = ref 1;
  } in

  runq := t::!runq

let return () =
  raise Todo
