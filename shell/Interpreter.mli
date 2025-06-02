(*s: Interpreter.mli *)
(*s: signature [[Interpreter.interpret]] *)
(* Interpret one operation. Called from a loop in main(). *)
val interpret :
  < Cap.fork ; Cap.exec ; Cap.chdir ; Cap.exit ; Cap.open_in; .. > ->
  Opcode.operation ->
  unit
(*e: signature [[Interpreter.interpret]] *)
(*e: Interpreter.mli *)
