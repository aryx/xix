(*s: version_control/cmd_test.ml *)
(*s: copyright gut *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright gut *)
open Common

(*s: function Cmd_test.test_sha1 *)
(* see https://git-scm.com/book/en/v2/Git-Internals-Git-Objects *)
let test_sha1 () =
  let content = "what is up, doc?"
    (*"test content\n"  *)
  in
  let header = "blob 16\000" in
  let store = header ^ content in

  let sha = Sha1.sha1 store in
  pr (spf "len = %d, raw = %s" (String.length sha) sha);
  let hexsha = Hexsha.of_sha sha in
  pr (spf "len = %d, str = %s" (String.length hexsha) hexsha);
  ()
(*e: function Cmd_test.test_sha1 *)

(*s: constant Cmd_test.cmd *)
let cmd = { Cmd.
  name = "test";
  help = " ";
  options = [];
  f = (fun args ->
    match args with
    | ["sha1"] -> test_sha1 ()
    | _ -> failwith (spf "test command [%s] not supported"
                       (String.concat ";" args))
  );
}
(*e: constant Cmd_test.cmd *)

(*e: version_control/cmd_test.ml *)
