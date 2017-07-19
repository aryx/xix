(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 *
 * less: use nested modules for objects, index, refs below?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type t = {
  (* less: on bare repo, this could be None *)
  worktree: Common.filename;
  (* less: on bare repo this could be the toplevel dir *)
  dotgit: Common.filename;

  (* less: compression level? *)
}

let (/) = Filename.concat

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* for loose objects *)
let hexsha_to_filename r hexsha =
  let dir = String.sub hexsha 0 2 in
  let file = String.sub hexsha 2 (String.length hexsha - 2) in
  r.dotgit / "objects" / dir / file

let with_file_out_with_lock file f =
  raise Todo

(*****************************************************************************)
(* Repo create/open/close *)
(*****************************************************************************)

let init root =
  let dirs = [
    ".git";
    ".git/objects";
    ".git/refs";
    ".git/refs/heads";
    ".git/refs/tags";
    ".git/refs/remote";
    ".git/refs/remote/origin";
    ".git/hooks";
    ".git/info";
  ] in
  raise Todo
  (* less: create empty index? *)

let open_ root = 
  let path = root / ".git" in
  if Sys.file_exists path &&
     (Unix.stat path).Unix.st_kind = Unix.S_DIR
  then 
    { worktree = root;
      dotgit = path;
      (* less: initialize obj store and refs container? *)
      (* less: grafts, hooks *)
    }
  else failwith (spf "No git repository was found at %s" path)


let with_repo root =
  raise Todo

let clone r dst =
  raise Todo

(*****************************************************************************)
(* Objects *)
(*****************************************************************************)
let read_obj r h =
  (* todo: look for packed obj *)

  let path = h |> Hexsha.of_sha |> hexsha_to_filename r in
  path |> Common.with_file_in (fun ch ->
    ch |> IO.input_channel |> Objects.read
  )

let mem_obj r h =
  raise Todo

let write_obj r h obj =
  raise Todo

(*****************************************************************************)
(* Refs *)
(*****************************************************************************)
let read_ref r aref =
  raise Todo

let remove_ref r aref =
  raise Todo

let add_ref r aref refval =
  raise Todo

let follow_ref r aref =
  raise Todo

let test_and_set_ref r aref refval =
  raise Todo

(*****************************************************************************)
(* Index *)
(*****************************************************************************)
let read_index r =
  raise Todo

let write_index r idx =
  raise Todo

(*****************************************************************************)
(* Packs *)
(*****************************************************************************)
