(*s: Interpreter.mli *)
(*s: signature [[Interpreter.interpret_operation]] *)
(* Interpret one operation. Called from a loop in main(). *)
val interpret_operation :
  < Cap.fork ; Cap.exec ; Cap.chdir ; Cap.exit ; Cap.open_in; .. > ->
  Opcode.operation ->
  unit
(*e: signature [[Interpreter.interpret_operation]] *)
(*e: Interpreter.mli *)
