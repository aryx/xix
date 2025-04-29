
(* Interpret one operation. Called from a loop in main(). *)
val interpret: 
  < Cap.fork; Cap.exec; Cap.chdir; .. > ->
  Opcode.operation -> unit
