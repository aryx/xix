open Common

module R = Runtime

(* was called Xrdcmds *)
let op_REPL () =

  let t = R.cur () in

  (* todo: flush error and reset error count *)
  if !Flags.sflag && not (Status.truestatus()) 
  then pr2 (spf "status=%s" (Status.getstatus ()));

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
    | None -> Process.return ()
    | Some seq ->
        (* should contain an op_return *)
        let codevec = Compile.compile seq in
        let newt = R.mk_thread codevec 0 t.R.locals in
        R.runq := newt::!(R.runq);

        decr t.R.pc;
        (* when codevec does a op_return(), then interpreter loop
         * in main should call us back since the pc was decremented above
         *)

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
