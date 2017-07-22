(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

let cmd = { Cmd.
  name = "rm";
  help = "";
  (* todo: -f *)
  options = [];
  f = (fun args ->
    match args with
    | [] -> 
      (* less: print help message instead *)
      failwith "Nothing specified, nothing removed."
    | xs ->
      (* todo: allow git rm from different location *)
      let r = Repository.open_ "." in
      (* less: support absolute paths, directories *)
      let relpaths = xs |> List.map (fun path ->
        if Filename.is_relative path
        then path
        else failwith (spf "Not a relative path: %s" path)
      )
      in
      (* removing is simpler than adding; no need to add blobs in
       * the object store, so can just use functions from Index
       *)
      (* less: not super efficient, could use hashes to speedup things*)
      r.Repository.index <-
        relpaths |> List.fold_left (fun idx relpath ->
          (* todo: -f? remove also file *)
          Index.remove_entry idx relpath
        ) r.Repository.index;
      Repository.write_index r
  );
}
