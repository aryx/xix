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

(* todo: handle ^ like HEAD^, so need more complex objectish parser *)
type objectish =
  | ObjByRef of Refs.t
  | ObjByHex of Hexsha.t
  (* ObjByTag *)

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
  (* less: packed refs *)
  let file = ref_to_filename r aref in
  let ch = open_in file in
  let input = IO.input_channel ch in
  Refs.read input

let rec follow_ref r aref =
  (* less: check if depth > 5? *)
  try (
  let content = read_ref r aref in
  match content with
  | Refs.Hash sha -> [aref], Some sha
  | Refs.OtherRef refname ->
    let (xs, shaopt) = follow_ref r (Refs.Ref refname) in
    aref::xs, shaopt
  ) 
  (* inexistent ref file, can happen at the beginning when have .git/HEAD
   * pointing to an inexistent .git/refs/heads/master
   *)
  with Sys_error _ (* no such file or directory *) -> [aref], None

let add_ref_if_new r aref refval =
  let (refs, shaopt) = follow_ref r aref in
  if shaopt <> None
  then false
  else begin
    let lastref = List.hd (List.rev refs) in
    let file = ref_to_filename r lastref in
    (* todo: ensure dirname exists *)
    file |> with_file_out_with_lock (fun ch ->
      (* todo: check file does not exist aleady *)
      ch |> IO.output_channel |> IO_.with_close_out (Refs.write refval)
    );
    true
  end

let set_ref_if_same_old r aref oldh newh =
  let (refs, _) = follow_ref r aref in
  let lastref = List.hd (List.rev refs) in
  let file = ref_to_filename r lastref in
  try 
    file |> with_file_out_with_lock (fun ch ->
      (* TODO generate some IO.No_more_input 
      let prev = read_ref r lastref in
      if prev <> (Refs.Hash oldh)
      then raise Not_found
      else 
      *)
        ch |> IO.output_channel |> IO_.with_close_out 
            (Refs.write (Refs.Hash newh))
    );
    true
  with Not_found -> false

let all_refs r =
  []

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

let read_objectish r objectish =
  match objectish with
  | ObjByRef aref -> 
    (match follow_ref r aref |> snd with
    | None -> failwith (spf "could not resolve %s" (Refs.string_of_ref aref))
    | Some sha -> 
      read_obj r sha
    )
  | ObjByHex hexsha ->
    let sha = Hexsha.to_sha hexsha in
    read_obj r sha

let add_obj r obj =
  let bytes = 
    IO.output_bytes () |> IO_.with_close_out (Objects.write obj) in
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

(*****************************************************************************)
(* Index *)
(*****************************************************************************)
let read_index r =
  r.index

let write_index r =
  let path = index_to_filename r in
  path |> with_file_out_with_lock (fun ch ->
    ch |> IO.output_channel |> IO_.with_close_out (Index.write r.index)
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
    let entry = Index.mk_entry relpath sha stat in
    r.index <- Index.add_entry r.index entry;
  );
  write_index r

(*****************************************************************************)
(* Commit *)
(*****************************************************************************)

let commit_index r author committer message =
  let aref = Refs.Head in
  let tree = Index.tree_of_index r.index 
    (fun t -> add_obj r (Objects.Tree t)) 
  in
  (* todo: execute pre-commit hook *)

  (* less: Try to read commit message from .git/MERGE_MSG *)
  let message = message in
  (* todo: execute commit-msg hook *)

  let commit = { Commit. parents = []; tree; author; committer; message } in

  let ok =
    match follow_ref r aref |> snd with
    | Some old_head ->
      (* less: merge_heads from .git/MERGE_HEADS *)
      let merge_heads = [] in
      let commit = { commit with Commit.parents = old_head :: merge_heads } in
      let sha = add_obj r (Objects.Commit commit) in
      set_ref_if_same_old r aref old_head sha
    | None ->
      (* maybe first commit so refs/heads/master may not even exist yet *)
      let commit = { commit with Commit.parents = [] } in
      let sha = add_obj r (Objects.Commit commit) in
      add_ref_if_new r aref (Refs.Hash sha)
  in
  if not ok
  then failwith (spf "%s changed during commit" (Refs.string_of_ref aref));
  (* todo: execute post-commit hook *)
  ()
  

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
  add_ref_if_new r Refs.Head Refs.default_head_content |> ignore;

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

