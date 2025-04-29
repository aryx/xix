
(* Interpret one operation. Called from a loop in main(). *)
val interpret: 
  < Cap.fork; Cap.exec; Cap.chdir; Cap.exit; .. > ->
  Opcode.operation -> unit
