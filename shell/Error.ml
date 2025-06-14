(*s: shell/Error.ml *)
module R = Runtime

(*s: function [[Error.error]] *)
(* less: error1 similar to error but without %r *)
let error (caps: < Cap.exit; ..>) (s : string) =
  (* less: use argv0 *)
  (* less: use %r *)
  Logs.err (fun m -> m "rc: %s" s);

  Status.setstatus "error";

  while (R.cur ()).R.iflag do
    (* goes up the call stack, like when we have an exception *)
    Process.return caps ();
  done
(*e: function [[Error.error]] *)
(*e: shell/Error.ml *)
