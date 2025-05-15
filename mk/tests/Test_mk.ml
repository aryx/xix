open Common
open Xix_mk

let t = Testo.create

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Regression tests for mk *)

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
      print_string (spf "executing: mk %s\n" cmd);
      try 
        CLI.main caps (Array.of_list ("mk" :: args)) |> ignore
      with Exit.ExitCode 0 -> ()
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
  Testo.categorize_suites "mk" [
      e2e_tests caps;
  ]
