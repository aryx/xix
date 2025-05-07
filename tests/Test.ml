
(*****************************************************************************)
(* Testsuite *)
(*****************************************************************************)

let test_hello =
  Testo.create "hello"
    (fun () -> print_endline "hello!")

let tests _env = [
  test_hello;
  ]


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (_caps : Cap.all_caps) : unit =
  Testo.interpret_argv ~project_name:"xix"
(*
    ~handle_subcommand_result:(fun exit_code res ->
      handle_results res;
      (* like in UCommon.main_boilerplate finalizer *)
      (* CapTmp.erase_temp_files caps#tmp; *)
      exit exit_code)

    (get_tests (caps :> Cap.all_caps));
 *)
  tests
  (* never reached *)

let () = Cap.main (fun all_caps -> main all_caps)
