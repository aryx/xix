(*s: version_control/cmd_dump.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common
open Fpath_.Operators

(*s: constant [[Cmd_dump.raw]] *)
let raw = ref false
(*e: constant [[Cmd_dump.raw]] *)
(*s: constant [[Cmd_dump.index]] *)
let index = ref false
(*e: constant [[Cmd_dump.index]] *)

(*s: function [[Cmd_dump.dump_object]] *)
(* =~ git cat-file -p *)
let dump_object (_caps : < Cap.open_in; ..>) (file : Fpath.t) =
  let chan = open_in !!file in
  let input = IO.input_channel chan in
  let unzipped = Unzip.inflate input in
  
  try
    if !raw
    then 
      let str = IO.read_all unzipped in 
      Logs.app (fun m -> m "%s" str)
    else begin
      let obj = Objects.read unzipped in
      let v = Dump.vof_obj obj in
      UConsole.print (OCaml.string_of_v v)
    end
  with Unzip.Error _err ->
    failwith "unzip error"
(*e: function [[Cmd_dump.dump_object]] *)

(*s: function [[Cmd_dump.dump_index]] *)
(* =~ dulwich dump-index, =~ git ls-files --stage *)
let dump_index (_caps : < Cap.open_in; ..>) (file : Fpath.t) =
  let chan = open_in !!file in
  let input = IO.input_channel chan in
  let index = Index.read input in
  let v = Dump.vof_index index in
  UConsole.print (OCaml.string_of_v v)
(*e: function [[Cmd_dump.dump_index]] *)

(*s: function [[Cmd_dump.dump]] *)
let dump caps (file : Fpath.t) =
  if !index
  then dump_index caps file
  else dump_object caps file
(*e: function [[Cmd_dump.dump]] *)

(*s: constant [[Cmd_dump.cmd]] *)
let cmd = { Cmd_.
  name = "dump";
  usage = " <file>";
  options = [
    "-raw", Arg.Set raw, " do not pretty print";
    "-index", Arg.Set index, " pretty print index content";
  ];
  f = (fun caps args ->
    match args with
    | [file] -> dump caps (Fpath.v file)
    | _ -> failwith (spf "dump command [%s] not supported"
                       (String.concat ";" args))
  );
}
(*e: constant [[Cmd_dump.cmd]] *)
(*e: version_control/cmd_dump.ml *)
