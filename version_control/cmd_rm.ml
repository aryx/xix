(*s: version_control/cmd_rm.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*s: function Cmd_rm.rm *)
let rm r relpaths =
  (* removing is simpler than adding; no need to add blobs in
   * the object store, so can just use functions from Index
   *)
  (* less: not super efficient, could use hashes to speedup things *)
  r.Repository.index <-
    relpaths |> List.fold_left (fun idx relpath ->
          (* todo: -f? remove also file *)
      Index.remove_entry idx relpath
    ) r.Repository.index;
  Repository.write_index r
(*e: function Cmd_rm.rm *)

(*s: constant Cmd_rm.cmd *)
let cmd = { Cmd.
  name = "rm";
  help = " [options] <file>...";
  options = [
  (* less: -f force, -r recursive, --quiet *)
  ];
  f = (fun args ->
    match args with
    | [] -> raise Cmd.ShowUsage
    | xs ->
      let r, relpaths = Repository.find_root_open_and_adjust_paths xs in
      rm r relpaths
  );
}
(*e: constant Cmd_rm.cmd *)
(*e: version_control/cmd_rm.ml *)
