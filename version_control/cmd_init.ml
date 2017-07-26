(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(* less: let bare = ref false *)

let cmd = { Cmd.
  name = "init";
  help = " [directory]";
  options = [
   (* less: -bare, --quiet *)
  ];
  f = (fun args ->
    match args with
    | [] ->
      Repository.init "."
    | [dir] ->
      Repository.init dir
    | _ -> raise Cmd.ShowUsage
  );
}
