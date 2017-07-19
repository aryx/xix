(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

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

let cmd = { Cmd.
  name = "test";
  help = "";
  options = [];
  f = (fun args ->
    match args with
    | ["sha1"] -> test_sha1 ()
    | _ -> failwith (spf "test command [%s] not supported"
                       (String.concat ";" args))
  );
}

