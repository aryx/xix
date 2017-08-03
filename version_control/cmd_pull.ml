(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let pull r path =
  
  raise Todo

let cmd = { Cmd.
  name = "pull";
  help = " [options] <repository>";
  options = [
  ];
  f = (fun args ->
    (* todo: allow git rm from different location *)
    let r = Repository.open_ "." in
    match args with
    | [path] -> 
      pull r path
    | _ -> raise Cmd.ShowUsage
  );
}
