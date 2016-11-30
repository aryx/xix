
exception Error2 of string * Location_cpp.loc * string * Location_cpp.loc   

(* raise Location_cpp.Error or Error2 exception *)
val check_program: Ast.program -> unit
