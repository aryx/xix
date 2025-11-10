(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Xix test suite entry point.
 *
 * From the root of the project you can do
 *
 *   $ ./test -s hello
 *
 * to run all the OCaml tests containing 'hello' in their test name.
 *)

(*****************************************************************************)
(* Testsuite *)
(*****************************************************************************)

let test_hello =
  Testo.create "hello"
    (fun () -> print_endline "hello!")

let tests caps _env =
  List.flatten [
    [ test_hello; ];
    Test_mk.tests caps;
    Test_shell.tests caps;
    Test_re.tests caps;
  ]


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : Cap.all_caps) : unit =
  Testo.interpret_argv ~project_name:"xix"
(*
    ~handle_subcommand_result:(fun exit_code res ->
      handle_results res;
      (* like in UCommon.main_boilerplate finalizer *)
      (* CapTmp.erase_temp_files caps#tmp; *)
      exit exit_code)

    (get_tests (caps :> Cap.all_caps));
 *)
  (tests caps)
  (* never reached *)

let () = Cap.main (fun all_caps -> main all_caps)
