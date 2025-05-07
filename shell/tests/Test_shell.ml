open Common

let t = Testo.create

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Regression tests for rc *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let run_main (caps : <CLI.caps; ..>) (cmd : string) : unit =
  let args = String.split_on_char ' ' cmd in
  (* we run CLI.main () below in a child process because it modifies many globals
   * and we don't want to write code to reset those globals between two
   * tests; simpler to just fork.
   *)
  CapProcess.apply_in_child_process caps (fun () ->
      print_string (spf "executing: rc %s\n" cmd);
      CLI.main caps (Array.of_list ("rc" :: args))
   )
   ()

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
let e2e_tests caps = 
  Testo.categorize "e2e" [
    (* TODO: use Exit_code.ml to remove the need for End_of_file catch *)
    t ~checked_output:(Testo.stdxxx ()) "--help" (fun () ->
        try 
          run_main caps "--help"
        with
          End_of_file -> ()
    )
  ]

(*****************************************************************************)
(* The suite *)
(*****************************************************************************)

let tests caps =
  Testo.categorize_suites "shell" [
      e2e_tests caps;
  ]

