open Common

let t = Testo.create

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Regression tests for re *)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
let basic_tests _caps = 
  Testo.categorize "basic" [
(*
    t ~checked_output:(Testo.stdxxx ()) "--help" (fun () ->
        match run_main caps "--help" with
        | Ok Exit.OK -> ()
        | Ok x -> failwith (spf "unexpected exit: %s" (Exit.show x))
        | Error s -> failwith (spf "unexpected failure: %s" s)

    )
*)
    t "hello re" (fun () -> print_endline "hello!");
  ]

(*****************************************************************************)
(* The suite *)
(*****************************************************************************)

let tests caps =
  Testo.categorize_suites "re" [
      basic_tests caps;
  ]

