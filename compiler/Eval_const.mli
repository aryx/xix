(*s: Eval_const.mli *)

(*s: exception [[Eval_const.NotAConstant]] *)
exception NotAConstant
(*e: exception [[Eval_const.NotAConstant]] *)

(*s: type [[Eval_const.error]] *)
type error = Check.error
(*e: type [[Eval_const.error]] *)
(*s: exception [[Eval_const.Error]] *)
exception Error of error
(*e: exception [[Eval_const.Error]] *)

(*s: type [[Eval_const.integer]] *)
type integer = int
(*e: type [[Eval_const.integer]] *)
(*s: type [[Eval_const.env]] *)
type env = (Ast.fullname, integer * Type.integer_type) Hashtbl.t 
(*e: type [[Eval_const.env]] *)

(*s: signature [[Eval_const.eval]] *)
(* may raise NotAConstant or Error *)
val eval: env -> Ast.expr -> integer
(*e: signature [[Eval_const.eval]] *)
(*e: Eval_const.mli *)
