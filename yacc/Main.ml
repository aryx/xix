(*s: yacc/Main.ml *)

(*s: toplevel [[Main._1]](yacc) *)
let _ = 
(*
  Tests.test_lr0 ();
  Tests.test_first_follow ();
  Tests.test_slr ();
  Tests.test_lr_engine ();
*)
  (*Printexc.catch*) CLI.main (); 
  exit 0
(*e: toplevel [[Main._1]](yacc) *)
(*e: yacc/Main.ml *)
