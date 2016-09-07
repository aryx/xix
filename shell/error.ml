open Common

module R = Runtime

(* less: error1 similar to error but without %r *)
let error s =
  (* less: use argv0 *)
  (* less: use %r *)
  pr2 (spf "rc: %s" s);

  Status.setstatus "error";

  while (R.cur ()).R.iflag do
    R.return ();
  done

