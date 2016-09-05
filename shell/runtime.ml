
type varname = string

(* In RC the base value is really a list of value.
 * Single values are really considered just list with one element.
 *)
type value = string list

type var = { 
  mutable v: value option;
  (* less: changed: bool *)
}


type thread = {
  code: Opcode.codevec;
  pc: int ref;

  mutable argv: string list;
  mutable argv_stack: string list list;
  locals: (varname, var) Hashtbl.t;

  (* stdin by default (can be changed when do '. file'?? when eval) *)
  mutable chan: in_channel;
  (* to display a prompt or not *)
  mutable iflag: bool;

  (* for error reporting ("<stdin>" when reading from stdin) *)
  mutable file: Common.filename;
  line: int ref;
}

let (globals: (varname, var) Hashtbl.t) = 
  Hashtbl.create 101

(* less: could have also  
 *  let run = { code = boostrap; pc = 1; chan = stdin }
 * in addition to runq. Then no need cur ().
 *)
let runq = ref []

(* less: argv0 *)

let cur () =
  match !runq with
  | [] -> failwith "empty runq"
  | x::xs -> x


let push_list () =
  let t = cur () in
  t.argv_stack <- t.argv :: t.argv_stack;
  t.argv <- []

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
    file = "<stdin>";
    line = ref 1;
  } in

  runq := t::!runq

