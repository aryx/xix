(*s: Eval_const.ml *)
(* Copyright 2016, 2017 Yoann Padioleau, see copyright.txt *)
open Common

open Ast
module E = Check

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* less: return also float at some point? *)
(*s: type [[Eval_const.integer]] *)
type integer = int
(*e: type [[Eval_const.integer]] *)

(* less: could do that in rewrite.ml so no need to pass is to eval *)
(*s: type [[Eval_const.env]] *)
type env = (Ast.fullname, integer * Type.integer_type) Hashtbl.t 
(*e: type [[Eval_const.env]] *)

(*s: exception [[Eval_const.NotAConstant]] *)
exception NotAConstant
(*e: exception [[Eval_const.NotAConstant]] *)


(* less: could factorize things in error.ml? *)
(*s: type [[Eval_const.error]] *)
type error = Check.error
(*e: type [[Eval_const.error]] *)

(*s: exception [[Eval_const.Error]] *)
exception Error of error
(*e: exception [[Eval_const.Error]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[Eval_const.eval]] *)
(* stricter: I do not handle float constants for enums *)
let rec eval env e0 =
  match e0.e with
  (* todo: enough for big integers? 
   * todo: we should also return an inttype in addition to the integer value.
   *)
  | Int (s, _inttype) -> int_of_string s
  | Id fullname ->
     if Hashtbl.mem env fullname
     then
       let (i, _inttype) = Hashtbl.find env fullname in
       i
     else raise NotAConstant
  | Binary (e1, op, e2) ->
    let i1 = eval env e1 in
    let i2 = eval env e2 in
    (match op with
    | Arith op -> 
      (match op with
      | Plus -> i1 + i2
      | Minus -> i1 - i2
      | Mul -> i1 * i2
      | Div -> 
        (* stricter: error, not warning *)
        if i2 = 0 
        then raise (Error (E.Misc ("divide by zero", e0.e_loc)))
        else i1 / i2
      | Mod -> 
        if i2 = 0 
        then raise (Error (E.Misc ("modulo by zero", e0.e_loc)))
        else i1 mod i2
      | And -> i1 land i2
      | Or -> i1 lor i2
      | Xor -> i1 lxor i2
      | ShiftLeft -> i1 lsl i2
      (* less: could be asr! need type information! *)
      | ShiftRight -> i1 lsr i2
      )
    | Logical op ->
      (match op with
      | Eq    -> if i1 =  i2 then 1 else 0
      | NotEq -> if i1 <> i2 then 1 else 0
      | Inf   -> if i1 <  i2 then 1 else 0
      | Sup   -> if i1 >  i2 then 1 else 0
      | InfEq -> if i1 <= i2 then 1 else 0
      | SupEq -> if i1 >= i2 then 1 else 0
      | AndLog -> raise Todo
      | OrLog -> raise Todo
      )
    )
  | Unary (op, e) ->
    let i = eval env e in
    (match op with
    | UnPlus -> i
    | UnMinus -> - i
    | Tilde -> lnot i (* sure? *)
    | _ -> raise Todo
    )

  | _ -> 
    raise NotAConstant (* todo: more opporunities? *)
(*e: function [[Eval_const.eval]] *)
(*e: Eval_const.ml *)
