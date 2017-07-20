(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(* less: let bare = ref false *)

let cmd = { Cmd.
  name = "init";
  help = "";
  options = [(*less: -bare *)];
  f = (fun args ->
    match args with
    | [] ->
      Repository.init "."
    | [dir] ->
      Repository.init dir
    | _ -> failwith "init: too many arguments"
  );
}
