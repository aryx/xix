(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(* could be moved in graph.ml *)
(* opti? time cache, and flag to skip cache if want refresh *)
let timeof file =
  try 
    let stat = Unix.lstat file in
    Some (stat.Unix.st_mtime)
  with Unix.Unix_error (_, _, _) -> None

let str_of_time timeopt =
  match timeopt with
  | None -> "0"
  | Some t -> spf "%.1f" t

