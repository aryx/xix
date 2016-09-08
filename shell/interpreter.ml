open Common

module O = Opcode
module R = Runtime
module E = Error

open Opcode (* just for big dispatch error case below *)


let interpret operation =
  match operation with
  | O.REPL -> Op_repl.op_REPL ()

  | O.Simple -> Op_process.op_Simple ()

  | O.Return -> R.return ()
  | O.Exit -> raise Todo

  | O.Mark -> R.push_list ()
  | O.Word ->
      let t = R.cur () in
      let x = t.R.code.(!(t.R.pc)) in
      incr t.R.pc;
      (match x with
      | O.S s -> R.push_word s
      (* stricter *)
      | _ -> failwith (spf "was expecting a S, not %s" 
                         (Dumper.s_of_operation operation))
      )

  | O.Assign ->
      let t = R.cur () in
      let argv = t.R.argv in
      (match argv with
      | [varname] ->
          (* no call to globlist for varname as it can be "*" for $* *)
          (* less: deglob varname *)
          let v = Var.vlook varname in
          R.pop_list ();
          (* less: globlist *)
          let argv = t.R.argv in
          v.R.v <- Some argv;
          R.pop_list ();

      | _ -> E.error "variable name not singleton!"
      )

  | O.Pipe -> raise Todo
  | O.PipeWait -> raise Todo

  | (Popm|
     Count|Concatenate|Stringify|Glob|Dollar|Index|
     Local|Unlocal|Fn|DelFn|
     If|IfNot|Jump|Match|Case|For|Wastrue|Bang|False|True|
     Read|Write|ReadWrite|Append|Close|Dup|PipeFd|
     Error|Eflag|
     Subshell|Backquote|Async
    ) ->
    failwith ("TODO: " ^ Dumper.s_of_opcode (O.F operation))
