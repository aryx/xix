(*s: version_control/cmd_test.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

let (/) = Filename.concat

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

let test_unzip () =
  Unzip.debug := true;
  let dir = ".git/objects" in
  dir |> Repository.walk_dir (fun path dirs files ->
    files |> List.iter (fun file ->
      let file = path / file in
      pr file;
      let chan = open_in file in
      let input = IO.input_channel chan in
      let unzipped = Unzip.inflate input in
      let _str = IO.read_all unzipped in 
      ()
    )
  )


let test_diff file1 file2 =
  let read_all path = 
      path |> Common.with_file_in (fun ch ->
        ch |> IO.input_channel |> IO.read_all
      )
  in
  let content1 = read_all file1 in
  let content2 = read_all file2 in

  let diffs = Diffs.diff content1 content2 in
  if not (diffs |> List.for_all (function Diff.Equal _ -> true | _ -> false))
  then begin
    pr (spf "diff --git %s %s" file1 file2);
    (* less: display change of modes *)
    Diff_unified.show_unified_diff diffs
  end

let test_diff3 fileo filea fileb =
  let read_all path = 
      path |> Common.with_file_in (fun ch ->
        ch |> IO.input_channel |> IO.read_all
      )
  in
  let contento = read_all fileo in
  let contenta = read_all filea in
  let contentb = read_all fileb in

  let chunks = Diff3.diff3 contento contenta contentb in
  pr2_gen chunks

(*s: constant Cmd_test.cmd *)
let cmd = { Cmd.
  name = "test";
  help = " ";
  options = [];
  f = (fun args ->
    match args with
    | ["sha1"] -> test_sha1 ()
    | ["unzip"] -> test_unzip ()
    | ["diff";file1;file2] -> test_diff file1 file2
    | ["diff"] -> 
      failwith "missing arguments to diff (diff <file1> <file2>)"
    | ["diff3";file1;file2;file3] -> test_diff3 file1 file2 file3
    | ["diff3"] -> 
      failwith "missing arguments to diff3 (diff3 <orig> <filea> <fileb>)"
    | _ -> failwith (spf "test command [%s] not supported"
                       (String.concat ";" args))
  );
}
(*e: constant Cmd_test.cmd *)

(*e: version_control/cmd_test.ml *)
