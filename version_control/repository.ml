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

let ref_to_filename r aref =
  match aref with
  | Refs.Head -> r.dotgit / "HEAD"
  (* less: win32: should actually replace '/' in name *)
  | Refs.Ref name -> r.dotgit / name

let index_to_filename r =
  r.dotgit / "index"

(* todo: see code of _Gitfile.__init__ O_EXCL ... *)
let with_file_out_with_lock f file =
  (* todo: create .lock file and then rename *)
  Common.with_file_out f file

(*****************************************************************************)
(* Refs *)
(*****************************************************************************)
let read_ref r aref =
  raise Todo

let remove_ref r aref =
  raise Todo

let add_ref r aref refval =
  (* less: check refval? *)
  let file = ref_to_filename r aref in
  file |> with_file_out_with_lock (fun ch ->
    ch |> IO.output_channel |> IO_utils.with_close_out (Refs.write refval)
  )

(* old: called follow() in dulwich *)
let resolve_ref r aref =
  (* less: check if depth > 5? *)
  raise Todo

let test_and_set_ref r aref refval =
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
(* Index *)
(*****************************************************************************)
let read_index r =
  raise Todo

let write_index r idx =
  let path = index_to_filename r in
  path |> with_file_out_with_lock (fun ch ->
    ch |> IO.output_channel |> IO_utils.with_close_out (Index.write idx)
  )

(*****************************************************************************)
(* Packs *)
(*****************************************************************************)

(*****************************************************************************)
(* Repo create/open/close *)
(*****************************************************************************)

let init root =
  (* rwxr-x--- *)
  let perm = 0o750 in
  if not (Sys.file_exists root)
  then Unix.mkdir root perm;

  (* less: bare argument? so no .git/ prefix? *)
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
  dirs |> List.iter (fun dir ->
    (* less: exn if already there? *)
    Unix.mkdir (root / dir) perm;
  );
  let r = {
    worktree = root;
    dotgit = root / ".git";
  } in
  add_ref r Refs.Head Refs.default_head_content;

  (* stricter: git and dulwich do no create empty index but I think it
   * simplifies things by not having to check later if the index already exists
   * or not.
   *)
  write_index r Index.empty;

  (* less: config file, description, hooks, etc *)
  pr (spf "Initialized empty Git repository in %s" (root / ".git"))


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

