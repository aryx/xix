
exception NotAConstant

type error = Check.error
exception Error of error

type integer = int
type env = (Ast.fullname, integer * Type_.integer_type) Hashtbl.t 

(* may raise NotAConstant or Error *)
val eval: env -> Ast.expr -> integer
