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

  let lexbuf = Lexing.from_channel t.R.chan in

  try 
    let ast_opt = Parse.parse_line lexbuf in

    match ast_opt with
    | None -> 
        Op_control.op_return ()
    | Some seq ->
        (* should contain an op_return *)
(*
        let codevec = Compile.compile seq in
        decr t.R.pc;
        R.start codevec 0 t.R.locals
*)
        (* when codevec does a op_return(), then interpreter loop
         * in main should call us back since the pc was decremented above
         *)
         decr t.R.pc;
         pr2 (Dumper.s_of_cmd_sequence seq)

  with Failure s -> 
    (* todo: check signals  *)

    if not t.R.iflag
    (* less: was doing Xreturn originally *)
    then failwith s
    else begin
      pr2 s;
      (* go back for next command *)
      decr t.R.pc;
    end

let xrepl = op_repl, "repl"
