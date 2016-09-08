open Common

(* can be anything: "*", "1", "2", "foo" *)
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
  locals: (varname, var) Hashtbl.t;

  (* less: seems needed only for switch commands, so remove it? *)
  mutable argv_stack: (string list) list;

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

(* less: argv0 *)


(* less: could have also  'let cur = { code = boostrap; pc = 1; chan = stdin }'
 * in addition to runq. Then there will be no need for cur ().
 *)
let runq = ref []

let cur () =
  match !runq with
  | [] -> failwith "empty runq"
  | x::xs -> x

(* I think we could get rid of that when we remove argv_stack *)
let push_list () =
  let t = cur () in
  t.argv_stack <- t.argv :: t.argv_stack;
  t.argv <- []

(* I think we could get rid of that when we remove argv_stack *)
let pop_list () =
  let t = cur () in
  match t.argv_stack with
  | [] -> failwith "pop_list but no argv"
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


(* starts with pc <> 0 when handle async, traps, pipes, etc  *)
let start code pc locals =
  let t = {
    code = code;
    pc = ref pc;
    argv = [];
    argv_stack = [];
    locals = locals;

    chan = stdin;
    iflag = false;
    file = None;
    line = ref 1;
  } in

  runq := t::!runq


(* todo: more stuff? *)
let exit _s =
  (* todo: how communicate error to parent process under Unix? *)
  exit (-2)


let return () =
  (* todo: turfredir() *)
  match !runq with
  | [] -> failwith "empty runq"
  | [x] -> exit "TODO: getstatus()"
  | x::xs -> 
      runq := xs

