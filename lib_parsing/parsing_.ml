(*s: stdlib/parsing_.ml *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* The parsing engine *)

open Lexing

(*s: type Parsing.parser_env (yacc) *)
(* Internal interface to the parsing engine *)

type parser_env =
  { mutable s_stack : int array;        (* States *)
    mutable v_stack : Obj.t array;      (* Semantic attributes *)

    mutable symb_start_stack : int array; (* Start positions *)
    mutable symb_end_stack : int array;   (* End positions *)

    mutable stacksize : int;            (* Size of the stacks *)
    mutable stackbase : int;            (* Base sp for current parse *)

    mutable curr_char : int;            (* Last token read *)
    mutable lval : Obj.t;               (* Its semantic attribute *)

    mutable symb_start : int;           (* Start pos. of the current symbol*)
    mutable symb_end : int;             (* End pos. of the current symbol *)

    mutable asp : int;                  (* The stack pointer for attributes *)
    mutable rule_len : int;             (* Number of rhs items in the rule *)
    mutable rule_number : int;          (* Rule number to reduce by *)

    mutable sp : int;                   (* Saved sp for parse_engine *)
    mutable state : int;                (* Saved state for parse_engine *)
    mutable errflag : int }             (* Saved error flag for parse_engine *)
(*e: type Parsing.parser_env (yacc) *)

(*s: type Parsing.parse_tables (stdlib/parsing.ml) (yacc) *)
(* coupling: parse_tables and parsing.c parse_tables must match! *)

type parse_tables =
  { actions : (parser_env -> Obj.t) array;
    transl_const : int array;
    transl_block : int array;
    lhs : string;
    len : string;
    defred : string;
    dgoto : string;
    sindex : string;
    rindex : string;
    gindex : string;
    tablesize : int;
    table : string;
    check : string;
    error_function : string -> unit }
(*e: type Parsing.parse_tables (stdlib/parsing.ml) (yacc) *)

(*s: exception Parsing.YYexit (stdlib/parsing.ml) (yacc) *)
exception YYexit of Obj.t
(*e: exception Parsing.YYexit (stdlib/parsing.ml) (yacc) *)
(*s: exception Parsing.Parse_error (stdlib/parsing.ml) (yacc) *)
exception Parse_error
(*e: exception Parsing.Parse_error (stdlib/parsing.ml) (yacc) *)

(*s: type Parsing.parser_input (yacc) *)
type parser_input =
    Start
  | Token_read
  | Stacks_grown_1
  | Stacks_grown_2
  | Semantic_action_computed
  | Error_detected
(*e: type Parsing.parser_input (yacc) *)

(*s: type Parsing.parser_output (yacc) *)
type parser_output =
    Read_token
  | Raise_parse_error
  | Grow_stacks_1
  | Grow_stacks_2
  | Compute_semantic_action
  | Call_error_function
(*e: type Parsing.parser_output (yacc) *)

(*
external parse_engine :
    parse_tables -> parser_env -> parser_input -> Obj.t -> parser_output
    = "parse_engine"
*)
let parse_engine a b c d =
  failwith "use yyparse_simple or Parsing module"

(*s: constant Parsing.env (yacc) *)
let env =
  { s_stack = Array.create 100 0;
    v_stack = Array.create 100 (Obj.repr ());
    symb_start_stack = Array.create 100 0;
    symb_end_stack = Array.create 100 0;
    stacksize = 100;
    stackbase = 0;
    curr_char = 0;
    lval = Obj.repr ();
    symb_start = 0;
    symb_end = 0;
    asp = 0;
    rule_len = 0;
    rule_number = 0;
    sp = 0;
    state = 0;
    errflag = 0 }
(*e: constant Parsing.env (yacc) *)

(*s: function Parsing.grow_stacks (yacc) *)
let grow_stacks() =
  let oldsize = env.stacksize in
  let newsize = oldsize * 2 in
  let new_s = Array.create newsize 0
  and new_v = Array.create newsize (Obj.repr ())
  and new_start = Array.create newsize 0
  and new_end = Array.create newsize 0 in
    Array.blit env.s_stack 0 new_s 0 oldsize;
    env.s_stack <- new_s;
    Array.blit env.v_stack 0 new_v 0 oldsize;
    env.v_stack <- new_v;
    Array.blit env.symb_start_stack 0 new_start 0 oldsize;
    env.symb_start_stack <- new_start;
    Array.blit env.symb_end_stack 0 new_end 0 oldsize;
    env.symb_end_stack <- new_end;
    env.stacksize <- newsize
(*e: function Parsing.grow_stacks (yacc) *)

(*s: function Parsing.clear_parser (yacc) *)
let clear_parser() =
  Array.fill env.v_stack 0 env.stacksize (Obj.repr ());
  env.lval <- Obj.repr ()
(*e: function Parsing.clear_parser (yacc) *)

(*s: constant Parsing.current_lookahead_fun (yacc) *)
let current_lookahead_fun = ref (fun (x: Obj.t) -> false)
(*e: constant Parsing.current_lookahead_fun (yacc) *)

(*s: function Parsing.yyparse (yacc) *)
let yyparse tables start lexer lexbuf =
  let rec loop cmd arg =
    match parse_engine tables env cmd arg with
      Read_token ->
        let t = Obj.repr(lexer lexbuf) in
        env.symb_start <- lexbuf.lex_abs_pos + lexbuf.lex_start_pos;
        env.symb_end   <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos;
        loop Token_read t
    | Raise_parse_error ->
        raise Parse_error
    | Compute_semantic_action ->
        let (action, value) =
          try
            (Semantic_action_computed, tables.actions.(env.rule_number) env)
          with Parse_error ->
            (Error_detected, Obj.repr ()) in
        loop action value
    | Grow_stacks_1 ->
        grow_stacks(); loop Stacks_grown_1 (Obj.repr ())
    | Grow_stacks_2 ->
        grow_stacks(); loop Stacks_grown_2 (Obj.repr ())
    | Call_error_function ->
        tables.error_function "syntax error";
        loop Error_detected (Obj.repr ()) in
  let init_asp = env.asp
  and init_sp = env.sp
  and init_stackbase = env.stackbase
  and init_state = env.state
  and init_curr_char = env.curr_char
  and init_errflag = env.errflag in
  env.stackbase <- env.sp + 1;
  env.curr_char <- start;
  try
    loop Start (Obj.repr ())
  with exn ->
    let curr_char = env.curr_char in
    env.asp <- init_asp;
    env.sp <- init_sp;
    env.stackbase <- init_stackbase;
    env.state <- init_state;
    env.curr_char <- init_curr_char;
    env.errflag <- init_errflag;
    match exn with
      YYexit v ->
        Obj.magic v
    | _ ->
        current_lookahead_fun :=
          (fun tok ->
            if Obj.is_block tok
            then tables.transl_block.(Obj.tag tok) = curr_char
            else tables.transl_const.(Obj.magic tok) = curr_char);
        raise exn
(*e: function Parsing.yyparse (yacc) *)

(*s: function Parsing.peek_val (yacc) *)
let peek_val env n =
  Obj.magic env.v_stack.(env.asp - n)
(*e: function Parsing.peek_val (yacc) *)

(*s: function Parsing.symbol_start (yacc) *)
let symbol_start () =
  if env.rule_len > 0
  then env.symb_start_stack.(env.asp - env.rule_len + 1)
  else env.symb_end_stack.(env.asp)
(*e: function Parsing.symbol_start (yacc) *)
(*s: function Parsing.symbol_end (yacc) *)
let symbol_end () =
  env.symb_end_stack.(env.asp)
(*e: function Parsing.symbol_end (yacc) *)

(*s: function Parsing.rhs_start (yacc) *)
let rhs_start n =
  env.symb_start_stack.(env.asp - (env.rule_len - n))
(*e: function Parsing.rhs_start (yacc) *)
(*s: function Parsing.rhs_end (yacc) *)
let rhs_end n =
  env.symb_end_stack.(env.asp - (env.rule_len - n))
(*e: function Parsing.rhs_end (yacc) *)

(*s: function Parsing.is_current_lookahead (yacc) *)
let is_current_lookahead tok =
  (!current_lookahead_fun)(Obj.repr tok)
(*e: function Parsing.is_current_lookahead (yacc) *)

(*s: function Parsing.parse_error (yacc) *)
let parse_error (msg: string) = ()
(*e: function Parsing.parse_error (yacc) *)


(*****************************************************************************)
(* Helpers for parsers using the simple code generation method *)
(*****************************************************************************)

(*s: type Parsing.stateid (yacc) *)
type stateid = S of int
(*e: type Parsing.stateid (yacc) *)
(*s: type Parsing.nonterm (yacc) *)
(* less: could be an index, but easier for debug to use the original name *)
type nonterm = NT of string
(*e: type Parsing.nonterm (yacc) *)
(*s: type Parsing.rule_action (yacc) *)
(* index in the rule actions table passed to yyparse *)
type rule_action = RA of int
(*e: type Parsing.rule_action (yacc) *)

(*s: type Parsing.action (yacc) *)
type action = 
  | Shift of stateid
  | Reduce of nonterm * int (* size of rhs of the rule *) * rule_action
  | Accept
(*e: type Parsing.action (yacc) *)

(*s: type Parsing.lr_tables (yacc) *)
type 'tok lr_tables = {
  action: stateid * 'tok -> action;
  goto: stateid * nonterm -> stateid;
}
(*e: type Parsing.lr_tables (yacc) *)

module Stack = Stack_

(*s: type Parsing.parser_env_simple (yacc) *)
type parser_env_simple = {
  states: stateid Stack.t;
  (* todo: opti: could use a growing array as one oftens needs to index it
   * with the peek_val
   * The semantic attributes (token value or non terminal value).
   *)
  values: Obj.t Stack.t;
  mutable current_rule_len: int;
}
(*e: type Parsing.parser_env_simple (yacc) *)

(*s: type Parsing.rules_actions (yacc) *)
type rules_actions = (parser_env_simple -> Obj.t) array
(*e: type Parsing.rules_actions (yacc) *)

(*s: constant Parsing.spf (yacc) *)
let spf = Printf.sprintf
(*e: constant Parsing.spf (yacc) *)

(*s: constant Parsing.debug (yacc) *)
let debug = ref true
(*e: constant Parsing.debug (yacc) *)
(*s: function Parsing.log (yacc) *)
let log x = 
  if !debug
  then begin
    print_endline ("YACC: " ^ x); flush stdout
  end
(*e: function Parsing.log (yacc) *)

  

(*s: function Parsing.peek_val_simple (yacc) *)
let peek_val_simple env i =
  if i < 1 && i >= env.current_rule_len
  then failwith (spf "peek_val_simple invalid argument %d" i)
  else Obj.magic (Stack.nth (env.current_rule_len - i) env.values)
(*e: function Parsing.peek_val_simple (yacc) *)

(*s: function Parsing.value_of_tok (yacc) *)
(* hmm, imitate what is done in parsing.c. A big ugly but tricky
 * to do otherwise and have a generic LR parsing engine.
 *)
let value_of_tok t =
  if Obj.is_block t
  then Obj.field t 0
  else Obj.repr ()
(*e: function Parsing.value_of_tok (yacc) *)

(*s: function Parsing.yyparse_simple (yacc) *)
let yyparse_simple lrtables rules_actions lexfun string_of_tok lexbuf =

  let env = {
    states = Stack.create ();
    values = Stack.create ();
    current_rule_len = 0;
  }
  in
  
  env.states |> Stack.push (S 0);
  let a = ref (lexfun lexbuf) in

  let finished = ref false in
  let res = ref (Obj.repr ()) in
  while not !finished do
    
    let s = Stack.top env.states in
    log (spf "state %d, tok = %s" (let (S x) = s in x) (string_of_tok !a));

    match lrtables.action (s, !a) with
    | Shift t ->
        log (spf "shift to %d" (let (S x) = t in x));
        env.states |> Stack.push t;
        env.values |> Stack.push (value_of_tok (Obj.repr !a));
        a := lexfun lexbuf;
    | Reduce (nt, n, ra) ->
        for i = 1 to n do
          Stack.pop env.states |> ignore
        done;
        let s = Stack.top env.states in
        env.states |> Stack.push (lrtables.goto (s, nt));
        let (NT ntstr) = nt in
        let (RA raidx) = ra in
        env.current_rule_len <- n;
        let v = rules_actions.(raidx) env in
        for i = 1 to n do
          Stack.pop env.values |> ignore
        done;
        env.values |> Stack.push v;
        log (spf "reduce %s, ra = %d" ntstr raidx);
    | Accept ->
        log "done!";
        finished := true;
        res := Stack.top env.values;
  done;
  Obj.magic !res
(*e: function Parsing.yyparse_simple (yacc) *)
  
(*e: stdlib/parsing_.ml *)
