(*s: version_control/cmd_test.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Algorithm tests *)
(*****************************************************************************)

(*s: function [[Cmd_test.test_sha1]] *)
(* see https://git-scm.com/book/en/v2/Git-Internals-Git-Objects *)
let test_sha1 (caps: < Cap.stdout; ..>) (content : string) =
  let header = spf "blob %d\000" (String.length content) in
  let store = header ^ content in

  let sha = Sha1.sha1 store in
  Console.print caps (spf "len = %d, raw = %s" (String.length sha) sha);
  let hexsha = Hexsha.of_sha sha in
  Console.print caps (spf "len = %d, str = %s" (String.length hexsha) hexsha);
  ()
(*e: function [[Cmd_test.test_sha1]] *)

let test_diff (caps : < Cap.open_in; Cap.stdout; ..>) 
    (file1 : Fpath.t) (file2 : Fpath.t) =
  let read_all path =
      path |> FS.with_open_in caps (fun (ch : Chan.i) ->
        ch.ic |> IO.input_channel |> IO.read_all
      )
  in
  let content1 = read_all file1 in
  let content2 = read_all file2 in

  let diffs = Diffs.diff content1 content2 in
  if not (diffs |> List.for_all (function Diff.Equal _ -> true | _ -> false))
  then begin
    Console.print caps (spf "diff --git %s %s" !!file1 !!file2);
    (* less: display change of modes *)
    Diff_unified.show_unified_diff diffs
  end

let test_diff3 (caps : < Cap.open_in; Cap.stdout; ..>)
      (fileo : Fpath.t) (filea : Fpath.t) (fileb : Fpath.t) =
  let read_all path =
      path |> FS.with_open_in caps (fun (ch : Chan.i) ->
        ch.ic |> IO.input_channel |> IO.read_all
      )
  in
  let contento = read_all fileo in
  let contenta = read_all filea in
  let contentb = read_all fileb in

  let chunks = Diff3.diff3 contento contenta contentb in
  let str = Diff3.merge !!filea !!fileb chunks in
  Console.print caps str

let test_unzip caps (file : Fpath.t) =
 file |> FS.with_open_in caps (fun (chan: Chan.i) ->
  let input = IO.input_channel chan.ic in
  let unzipped = Unzip.inflate input in
  let str = IO.read_all unzipped in
  Console.print caps str
 )

let test_zip caps (file : Fpath.t) =
 file |> FS.with_open_in caps (fun (chan: Chan.i) ->
  let _input = IO.input_channel chan.ic in
  let zipped = 
    (* Zip.deflate input *)
    failwith "Zip.deflate"
  in
  let str = IO.read_all zipped in
  Console.print caps str
 )
(* TODO
  let dst = file ^ ".deflate" in
  let chan = open_out dst in
  let output = IO.output_channel chan in
  IO.nwrite_string output str;
  IO.close_out output
*)
  

let test_unzip_all_objects caps () =
  Unzip.debug := true;
  let dir = Fpath.v ".git/objects" in
  dir |> Repository.walk_dir (fun path _dirs files ->
    files |> List.iter (fun file ->
      let file = path / file in
      Console.print caps !!file;
      file |> FS.with_open_in caps (fun (chan : Chan.i) ->
       let input = IO.input_channel chan.ic in
       let unzipped = Unzip.inflate input in
       let _str = IO.read_all unzipped in 
       ()
      )
    )
  )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: constant [[Cmd_test.cmd]] *)
let cmd = { Cmd_.
  name = "test";
  usage = " ";
  options = [];
  f = (fun caps args ->
    match args with
    | ["sha1"; str] -> test_sha1 caps str
    | ["sha1"] -> test_sha1 caps "what is up, doc?"

    | ["diff";file1;file2] -> test_diff caps (Fpath.v file1) (Fpath.v file2)
    | ["diff"] -> 
      failwith "missing arguments to diff (diff <file1> <file2>)"

    | ["diff3";file1;file2;file3] -> 
            test_diff3 caps (Fpath.v file1) (Fpath.v file2) (Fpath.v file3)
    | ["diff3"] -> 
      failwith "missing arguments to diff3 (diff3 <orig> <filea> <fileb>)"

    | ["unzip"; file] -> test_unzip caps (Fpath.v file)
    | ["zip"; file] -> test_zip caps (Fpath.v file)

    | ["unzip_all_objects"] -> test_unzip_all_objects caps ()

    | _ -> failwith (spf "test command [%s] not supported"
                       (String.concat ";" args))
  );
}
(*e: constant [[Cmd_test.cmd]] *)

(*e: version_control/cmd_test.ml *)
