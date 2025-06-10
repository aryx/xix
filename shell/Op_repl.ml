(*s: Op_repl.ml *)

module R = Runtime

(*s: function [[Op_repl.op_REPL]] *)
(* was called Xrdcmds *)
let op_REPL (caps : < Cap.exit; ..>) () =
  let t = R.cur () in

  (*s: [[Op_repl.op_REPL()]] if [[sflag]] *)
  (* todo: flush error and reset error count *)
  if !Flags.sflag && not (Status.truestatus()) 
  then Logs.app (fun m -> m "status=%s" (Status.getstatus ()));
  (*e: [[Op_repl.op_REPL()]] if [[sflag]] *)
  (*s: [[Op_repl.op_REPL()]] set [[prompt]] if [[iflag]] *)
  (* set prompstr *)
  if t.R.iflag then begin
    let promptv = (Var.vlook "prompt").R.v in
    Prompt.prompt := 
      (match promptv with
      | Some (x::_xs) -> x
      (* stricter? display error message if prompt set but no element?*)
      | Some [] | None -> "% "
      );
  end;
  (*e: [[Op_repl.op_REPL()]] set [[prompt]] if [[iflag]] *)
  (* less: call Noerror before yyparse *)

  let lexbuf = t.R.lexbuf in
  try 
    let cmdseq_opt = Parse.parse_line lexbuf in
    match cmdseq_opt with
    | Some seq ->
        (* should contain an op_return *)
        let codevec = Compile.compile seq in

        let newt = R.mk_thread codevec 0 t.R.locals in
        R.runq := newt::!(R.runq);

        decr t.R.pc;
        (* when codevec does a op_return(), then interpreter loop
         * in main should call us back since the pc was decremented above
         *)
    (*s: [[Op_repl.op_REPL()]] match [[cmdset_opt]] other cases *)
    | None -> Process.return caps ()
    (*e: [[Op_repl.op_REPL()]] match [[cmdset_opt]] other cases *)

  with Failure s -> 
    (*s: [[Op_repl.op_REPL()]] when [[Failure s]] thrown *)
    (* todo: check signals  *)
    (* less: was doing Xreturn originally *)
    if t.R.iflag
    then begin
      Logs.err (fun m -> m "%s" s);
      (* go back for next command *)
      decr t.R.pc;
    end
    else failwith s
    (*e: [[Op_repl.op_REPL()]] when [[Failure s]] thrown *)
(*e: function [[Op_repl.op_REPL]] *)
(*e: Op_repl.ml *)
