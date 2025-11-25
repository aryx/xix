open Common
open Fpath_.Operators
open Xix_compiler

let t = Testo.create

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Regression tests for rc *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let run_main (caps : <CLI.caps; ..>) (cmd : string) : (Exit.t, string) result =
  let args = String.split_on_char ' ' cmd in
  (* we run CLI.main () below in a child process because it modifies many globals
   * and we don't want to write code to reset those globals between two
   * tests; simpler to just fork.
   *)
  Proc.apply_in_child_process caps (fun () ->
      print_string (spf "executing: %s\n" cmd);
      try 
        Ok (Exit.catch (fun () -> 
              CLI.main caps (Array.of_list args)))
      with
      (* actually impossible *)
      | Failure s -> failwith (spf "impossible, failure %s should be catched" s)
      | End_of_file -> Error "End_of_file"          
   )
   ()

let ok_or_fail res =
  match res with
  | Ok Exit.OK -> ()
  | Ok x -> failwith (spf "unexpected exit: %s" (Exit.show x))
  | Error s -> failwith (spf "unexpected failure: %s" s)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
let e2e_tests caps = 
  Testo.categorize "e2e" [
    (* TODO: use Exit_code.ml to remove the need for End_of_file catch *)
    t ~checked_output:(Testo.stdxxx ()) "--help" (fun () ->
        run_main caps "o5c --help" |> ok_or_fail
    )
  ]

let codegen_tests caps =
  (* LATER: use Glob.glob "*.c" *)
  let files = [
    "trivial.c";

    "if.c";
    "while.c";
    "dowhile.c";
    "goto.c";
    "break.c";

    "simple.c";
    "assign.c";
    "function.c";
    "local.c";
    "params.c";
    "return.c";
    "call.c";

    "address_local.c";
    "enum.c";

(* TODO:
    "array.c";
    "pointer.c";

    "helloraw.c";

    "arithmetic.c";
*)
    ]
  in
  let archs = ["o5c"; "ovc"] in
  let tests =
    archs |> List.map (fun arch ->
        files |> List.map (fun file ->
            let path = Fpath.v "tests/compiler/codegen" / file in
            let testname = 
              if arch = "o5c" 
              (* to not change snapshot path for the o5c tests *)
              then !!path
              else spf "%s (%s)" !!path arch
            in
            t ~checked_output:(Testo.stdxxx ()) testname (fun () ->
                run_main caps (spf "%s -S %s" arch !!path) |> ok_or_fail
            )
       )) |> List.flatten
  in
  Testo.categorize "codegen" tests

(*****************************************************************************)
(* The suite *)
(*****************************************************************************)

let tests caps =
  Testo.categorize_suites "compiler" [
      e2e_tests caps;
      codegen_tests caps;
  ]

