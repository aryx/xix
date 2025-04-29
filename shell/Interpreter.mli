
(* Interpret one operation. Called from a loop in main(). *)
val interpret: 
  < Cap.fork; Cap.exec; .. > ->
  Opcode.operation -> unit
