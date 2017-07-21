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

  mutable index: Index.t;

  (* less: compression level config field? *)
}

let (/) = Filename.concat

(* rwxr-x--- *)
let dirperm = 0o750

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* for loose objects *)
let hexsha_to_filename r hexsha =
  let dir = String.sub hexsha 0 2 in
  let file = String.sub hexsha 2 (String.length hexsha - 2) in
  r.dotgit / "objects" / dir / file

let hexsha_to_dirname r hexsha =
  let dir = String.sub hexsha 0 2 in
  r.dotgit / "objects" / dir

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
    (* less: check read everything from channel? *)
    (* todo: check if sha consistent? *)
    ch |> IO.input_channel |> Unzip.inflate |> Objects.read
  )

let add_obj r obj =
  let bytes = 
    IO.output_bytes () |> IO_utils.with_close_out (Objects.write obj) in
  let sha = Sha1.sha1 bytes in
  let hexsha = Hexsha.of_sha sha in
  let dir = hexsha_to_dirname r hexsha in
  if not (Sys.file_exists dir)
  then Unix.mkdir dir dirperm;
  let file = hexsha_to_filename r hexsha in
  if (Sys.file_exists file)
  then sha (* deduplication! nothing to write, can share objects *)
  else begin
    file |> with_file_out_with_lock (fun ch ->
      let ic = IO.input_bytes bytes in
      let oc = IO.output_channel ch in
      Zlib.compress 
        (fun buf -> 
          try IO.input ic buf 0 (Bytes.length buf)
          with IO.No_more_input -> 0
        )
        (fun buf len -> 
          IO.output oc buf 0 len |> ignore);
      IO.close_out oc;
    );
    sha
  end

let mem_obj r h =
  raise Todo

(*****************************************************************************)
(* Index *)
(*****************************************************************************)
let read_index r =
  r.index

let write_index r =
  let path = index_to_filename r in
  path |> with_file_out_with_lock (fun ch ->
    ch |> IO.output_channel |> IO_utils.with_close_out (Index.write r.index)
  )

(* old: was called stage() in dulwich *)
let add_in_index r relpaths =
  assert (relpaths |> List.for_all Filename.is_relative);
  relpaths |> List.iter (fun relpath ->
    let full_path = r.worktree / relpath in
    let stat = 
      try Unix.lstat full_path 
      with Unix.Unix_error _ ->
        failwith (spf "Repository.add_in_index: %s does not exist anymore"
                    relpath)
    in
    let data = 
      match stat.Unix.st_kind with
      | Unix.S_REG -> 
        full_path |> Common.with_file_in (fun ch ->
          ch |> IO.input_channel |> IO.read_all
        )
      | Unix.S_LNK ->
        Unix.readlink full_path
      | _ -> failwith (spf "Repository.add_in_index: %s kind not handled" 
                         relpath)
    in
    let obj = Objects.Blob data in
    let sha = add_obj r obj in
    let entry = Index.entry_of_stat stat relpath sha in
    r.index <- Index.add r.index entry;
  );
  write_index r


(*****************************************************************************)
(* Packs *)
(*****************************************************************************)

(*****************************************************************************)
(* Repo init/open *)
(*****************************************************************************)

let init root =
  if not (Sys.file_exists root)
  then Unix.mkdir root dirperm;

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
    Unix.mkdir (root / dir) dirperm;
  );
  let r = {
    worktree = root;
    dotgit = root / ".git";
    index = Index.empty;
  } in
  add_ref r Refs.Head Refs.default_head_content;

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
      index = 
        if Sys.file_exists (path / "index")
        then 
          (path / "index") |> Common.with_file_in (fun ch ->
            ch |> IO.input_channel |> Index.read)
        else Index.empty
    }
  else failwith (spf "Not a git repository at %s" root)

(*****************************************************************************)
(* Cloning *)
(*****************************************************************************)

let clone r dst =
  raise Todo

