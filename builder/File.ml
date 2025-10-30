(*s: mk/File.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

(* could be moved in graph.ml *)
(*s: function [[File.timeof]] *)
(* opti? time cache, and flag to skip cache if want refresh *)
let timeof file =
  try 
    (* bugfix: use stat, not lstat, to get the time of what is pointed
     * by the symlink, not the symlink
     *)
    let stat = Unix.stat !!file in
    Some (stat.Unix.st_mtime)
  with Unix.Unix_error (_, _, _) -> None
(*e: function [[File.timeof]] *)

(*s: function [[File.str_of_time]] *)
let str_of_time timeopt =
  match timeopt with
  | None -> "0"
  | Some t -> spf "%.1f" t
(*e: function [[File.str_of_time]] *)
(*e: mk/File.ml *)
