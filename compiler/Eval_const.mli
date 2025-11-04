(*s: Eval_const.mli *)

(*s: exception [[Eval_const.NotAConstant (Eval_const.mli)]] *)
exception NotAConstant
(*e: exception [[Eval_const.NotAConstant (Eval_const.mli)]] *)

(*s: type [[Eval_const.error (Eval_const.mli)]] *)
type error = Check.error
(*e: type [[Eval_const.error (Eval_const.mli)]] *)
(*s: exception [[Eval_const.Error (Eval_const.mli)]] *)
exception Error of error
(*e: exception [[Eval_const.Error (Eval_const.mli)]] *)

(*s: type [[Eval_const.integer (Eval_const.mli)]] *)
type integer = int
(*e: type [[Eval_const.integer (Eval_const.mli)]] *)
(*s: type [[Eval_const.env (Eval_const.mli)]] *)
type env = (Ast.fullname, integer * Type.integer_type) Hashtbl.t 
(*e: type [[Eval_const.env (Eval_const.mli)]] *)

(*s: signature [[Eval_const.eval]] *)
(* may raise NotAConstant or Error *)
val eval: env -> Ast.expr -> integer
(*e: signature [[Eval_const.eval]] *)
(*e: Eval_const.mli *)
