open Common

module R = Runtime

(* was called Xrdcmds *)
let op_repl () =

  let t = R.cur () in

  (* todo: flush error and reset error count *)
  (* todo: print status if -s *)

  (* set prompstr *)
  if t.R.iflag then begin
    let promptv = (Var.vlook "prompt").R.v in
    Prompt.prompt := 
      (match promptv with
      | Some (x::xs) -> x
      (* stricter? display error message if prompt set but no element?*)
      | Some [] | None -> "% "
      );
  end;
  (* less: call Noerror before yyparse *)
  
  let ast_opt = Parse.parse_line t.R.chan in

  match ast_opt with
  | None -> 
      (* todo: error management if parse_line return error 
       * check for EOF here.
      *)
      raise Todo
  | Some ast ->
      (* should contain an op_return *)
      let codevec = Compile.compile ast in
      
      decr t.R.pc;
      R.start codevec 0 t.R.locals

  (* when codevec does a op_return(), then interpreter loop
   * in main should call us back since the pc was decremented above
   *)

let xrepl = op_repl, "repl"
