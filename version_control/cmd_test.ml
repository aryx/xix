(*s: version_control/cmd_test.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

let (|>) = Stdcompat.(|>)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (/) = Filename.concat

(*****************************************************************************)
(* Algorithm tests *)
(*****************************************************************************)

(*s: function [[Cmd_test.test_sha1]] *)
(* see https://git-scm.com/book/en/v2/Git-Internals-Git-Objects *)
let test_sha1 content =
  let header = spf "blob %d\000" (String.length content) in
  let store = header ^ content in

  let sha = Sha1.sha1 store in
  UConsole.print (spf "len = %d, raw = %s" (String.length sha) sha);
  let hexsha = Hexsha.of_sha sha in
  UConsole.print (spf "len = %d, str = %s" (String.length hexsha) hexsha);
  ()
(*e: function [[Cmd_test.test_sha1]] *)

let test_diff file1 file2 =
  let read_all path =
      let path = Fpath.v path in
      path |> UChan.with_open_in (fun (ch : Chan.i) ->
        ch.ic |> IO.input_channel |> IO.read_all
      )
  in
  let content1 = read_all file1 in
  let content2 = read_all file2 in

  let diffs = Diffs.diff content1 content2 in
  if not (diffs |> List.for_all (function Diff.Equal _ -> true | _ -> false))
  then begin
    UConsole.print (spf "diff --git %s %s" file1 file2);
    (* less: display change of modes *)
    Diff_unified.show_unified_diff diffs
  end

let test_diff3 fileo filea fileb =
  let read_all path =
      let path = Fpath.v path in
      path |> UChan.with_open_in (fun (ch : Chan.i) ->
        ch.ic |> IO.input_channel |> IO.read_all
      )
  in
  let contento = read_all fileo in
  let contenta = read_all filea in
  let contentb = read_all fileb in

  let chunks = Diff3.diff3 contento contenta contentb in
  let str = Diff3.merge filea fileb chunks in
  print_string str

let test_unzip file =
  let chan = open_in file in
  let input = IO.input_channel chan in
  let unzipped = Unzip.inflate input in
  let str = IO.read_all unzipped in
  print_string str

let test_zip file =
  let chan = open_in file in
  let input = IO.input_channel chan in
  let zipped = Zip.deflate input in
  let str = IO.read_all zipped in
  UConsole.print str
(* TODO
  let dst = file ^ ".deflate" in
  let chan = open_out dst in
  let output = IO.output_channel chan in
  IO.nwrite_string output str;
  IO.close_out output
*)
  

let test_unzip_all_objects () =
  Unzip.debug := true;
  let dir = ".git/objects" in
  dir |> Repository.walk_dir (fun path _dirs files ->
    files |> List.iter (fun file ->
      let file = path / file in
      UConsole.print file;
      let chan = open_in file in
      let input = IO.input_channel chan in
      let unzipped = Unzip.inflate input in
      let _str = IO.read_all unzipped in 
      ()
    )
  )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: constant [[Cmd_test.cmd]] *)
let cmd = { Cmd.
  name = "test";
  usage = " ";
  options = [];
  f = (fun args ->
    match args with
    | ["sha1"; str] -> test_sha1 str
    | ["sha1"] -> test_sha1 "what is up, doc?"

    | ["diff";file1;file2] -> test_diff file1 file2
    | ["diff"] -> 
      failwith "missing arguments to diff (diff <file1> <file2>)"

    | ["diff3";file1;file2;file3] -> test_diff3 file1 file2 file3
    | ["diff3"] -> 
      failwith "missing arguments to diff3 (diff3 <orig> <filea> <fileb>)"

    | ["unzip"; file] -> test_unzip file
    | ["zip"; file] -> test_zip file

    | ["unzip_all_objects"] -> test_unzip_all_objects ()

    | _ -> failwith (spf "test command [%s] not supported"
                       (String.concat ";" args))
  );
}
(*e: constant [[Cmd_test.cmd]] *)

(*e: version_control/cmd_test.ml *)
