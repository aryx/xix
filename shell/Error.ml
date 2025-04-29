open Common

module R = Runtime

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
