(*s: version_control/cmd_rm.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)

(*s: function [[Cmd_rm.rm]] *)
let rm r relpaths =
  (* less: not super efficient, could use hashes to speedup things *)
  r.Repository.index <-
    relpaths |> List.fold_left (fun idx relpath ->
          (* todo: -f? remove also file *)
      Index.remove_entry idx relpath
    ) r.Repository.index;
  Repository.write_index r
(*e: function [[Cmd_rm.rm]] *)

(*s: constant [[Cmd_rm.cmd]] *)
let cmd = { Cmd_.
  name = "rm";
  usage = " [options] <file>...";
  options = [
    (*s: [[Cmd_rm.cmd]] command-line options *)
    (* less: -f force, -r recursive, --quiet *)
    (*e: [[Cmd_rm.cmd]] command-line options *)
  ];
  f = (fun caps args ->
    match args with
    | [] -> raise Cmd_.ShowUsage
    | xs ->
      let r, relpaths = 
              Repository.find_root_open_and_adjust_paths caps
                (Fpath_.of_strings xs) in
      rm r relpaths
  );
}
(*e: constant [[Cmd_rm.cmd]] *)
(*e: version_control/cmd_rm.ml *)
