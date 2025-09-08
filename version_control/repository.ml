(*s: version_control/repository.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* API to access repository data (objects, index, refs, packs).
 *
 * less: use nested modules for objects, index, refs below?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Repository.t]] *)
type t = {
  worktree: Common.filename;
  dotgit: Common.filename; (* usually <worktree>/.git *)

  (*s: [[Repository.t]] index field *)
  mutable index: Index.t;
  (*e: [[Repository.t]] index field *)
}
(*e: type [[Repository.t]] *)

(*s: constant [[Repository.SlashOperator]] *)
let (/) = Filename.concat
(*e: constant [[Repository.SlashOperator]] *)

(*s: constant [[Repository.dirperm]] *)
(* rwxr-x--- *)
let dirperm = 0o750
(*e: constant [[Repository.dirperm]] *)

(*s: type [[Repository.objectish]] *)
(* todo: handle ^ like HEAD^, so need more complex objectish parser *)
type objectish =
  | ObjByRef of Refs.t
  | ObjByHex of Hexsha.t
  | ObjByBranch of string
  (*s: [[Repository.objectish]] cases *)
  (* todo:  ObjByShortHex *)
  (*x: [[Repository.objectish]] cases *)
  (* ObjByTag *)
  (*e: [[Repository.objectish]] cases *)
(*e: type [[Repository.objectish]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Repository.hexsha_to_filename]] *)
(* for loose objects *)
let hexsha_to_filename r hexsha =
  let dir = String.sub hexsha 0 2 in
  let file = String.sub hexsha 2 (String.length hexsha - 2) in
  r.dotgit / "objects" / dir / file
(*e: function [[Repository.hexsha_to_filename]] *)

(*s: function [[Repository.ref_to_filename]] *)
let ref_to_filename r aref =
  match aref with
  | Refs.Head -> r.dotgit / "HEAD"
  (* less: win32: should actually replace '/' in name *)
  | Refs.Ref name -> r.dotgit / name
(*e: function [[Repository.ref_to_filename]] *)

(*s: function [[Repository.index_to_filename]] *)
let index_to_filename r =
  r.dotgit / "index"
(*e: function [[Repository.index_to_filename]] *)

(*s: function [[Repository.with_file_out_with_lock]] *)
(* todo: see code of _Gitfile.__init__ O_EXCL ... *)
let with_file_out_with_lock f file =
  (* todo: create .lock file and then rename *)
  let file = Fpath.v file in
  UChan.with_open_out (fun (chan : Chan.o) -> f chan.oc) file
(*e: function [[Repository.with_file_out_with_lock]] *)


(* move in common.ml? *)
(*s: function [[Repository.with_opendir]] *)
(* less: use finalize *)
let with_opendir f dir =
  let handle = Unix.opendir dir in
  let res = f handle in
  Unix.closedir handle;
  res
(*e: function [[Repository.with_opendir]] *)
    
(* move in common.ml? (but remove .git specific stuff) *)
(*s: function [[Repository.walk_dir]] *)
(* inspired from os.path.walk in Python *)
let rec walk_dir f dir =
  dir |> with_opendir (fun handle ->
    let dirs = ref [] in
    let files = ref [] in
    try 
      while true do
        let s = Unix.readdir handle in
        (* git specific here *)
        if s <> "." && s <> ".." && s <> ".git" then begin
          let path = Filename.concat dir s in
          let st = Unix.lstat path in
          (match st.Unix.st_kind with
          | Unix.S_DIR -> Stack_.push s dirs
          | _ -> Stack_.push s files
          )
        end
      done
    with End_of_file ->
      let dirs = List.rev !dirs in
      let files = List.rev !files in
      f dir dirs files;
      dirs |> List.iter (fun s ->
        walk_dir f (Filename.concat dir s)
      )
  )
(*e: function [[Repository.walk_dir]] *)

(*****************************************************************************)
(* Refs *)
(*****************************************************************************)

(*s: function [[Repository.read_ref]] *)
let read_ref r aref =
  (* less: packed refs *)
  let file = ref_to_filename r aref in
  let file = Fpath.v file in
  file |> UChan.with_open_in (fun (ch : Chan.i) ->
    ch.ic |> IO.input_channel |> Refs.read
  )
(*e: function [[Repository.read_ref]] *)

(*s: function [[Repository.follow_ref]] *)
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
(*e: function [[Repository.follow_ref]] *)

(*s: function [[Repository.follow_ref_some]] *)
let follow_ref_some r aref =
  match follow_ref r aref |> snd with
  | Some sha -> sha
  | None -> failwith (spf "could not follow %s" (Refs.string_of_ref aref))
(*e: function [[Repository.follow_ref_some]] *)

(*s: function [[Repository.add_ref_if_new]] *)
let add_ref_if_new r aref refval =
  let (refs, shaopt) = follow_ref r aref in
  if shaopt <> None
  then false
  else begin
    let lastref = List.hd (List.rev refs) in
    let file = ref_to_filename r lastref in
    (* todo: ensure dirname exists *)
    file |> with_file_out_with_lock (fun ch ->
      (* todo: check file does not exist aleady; then return false! *)
      ch |> IO.output_channel |> IO_.with_close_out (Refs.write refval);
      true
    )
  end
(*e: function [[Repository.add_ref_if_new]] *)

(*s: function [[Repository.del_ref]] *)
let del_ref r aref =
  let file = ref_to_filename r aref in
  Unix.unlink file
(*e: function [[Repository.del_ref]] *)

(*s: function [[Repository.set_ref_if_same_old]] *)
let set_ref_if_same_old r aref _oldh newh =
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
(*e: function [[Repository.set_ref_if_same_old]] *)

(*s: function [[Repository.set_ref]] *)
let set_ref r aref newh =
  let (refs, _) = follow_ref r aref in
  let lastref = List.hd (List.rev refs) in
  let file = ref_to_filename r lastref in
  file |> with_file_out_with_lock (fun ch ->
    ch |> IO.output_channel |> IO_.with_close_out 
        (Refs.write (Refs.Hash newh))
  )
(*e: function [[Repository.set_ref]] *)
  

(*s: function [[Repository.write_ref]] *)
(* low-level *)
let write_ref r aref content =
  let file = ref_to_filename r aref in
  file |> with_file_out_with_lock (fun ch ->
    ch |> IO.output_channel |> IO_.with_close_out (Refs.write content))
(*e: function [[Repository.write_ref]] *)

(*s: function [[Repository.all_refs]] *)
let all_refs r =
  let root = r.dotgit ^ "/" in
  let rootlen = String.length root in
  let res = ref [] in
  (root / "refs") |> walk_dir (fun path _dirs files ->
    files |> List.iter (fun file ->
      (* less: replace os.path.sep *)
      let dir = String.sub path rootlen (String.length path - rootlen) in
      let refname = dir / file in
      Stack_.push refname res
    );
   );
  List.rev !res
(*e: function [[Repository.all_refs]] *)

(*****************************************************************************)
(* Objects *)
(*****************************************************************************)

(*s: function [[Repository.read_obj]] *)
let read_obj r h =
  (* todo: look for packed obj *)
  let path = h |> Hexsha.of_sha |> hexsha_to_filename r in
  let path = Fpath.v path in
  path |> UChan.with_open_in (fun (ch : Chan.i) ->
    (* less: check read everything from channel? *)
    (* todo: check if sha consistent? *)
    ch.ic |> IO.input_channel |> Compression.decompress |> Objects.read
  )
(*e: function [[Repository.read_obj]] *)

(*s: function [[Repository.read_commit]] *)
let read_commit r h =
  match read_obj r h with
  | Objects.Commit x -> x
  | _ -> failwith "read_commit: was expecting a commit"
(*e: function [[Repository.read_commit]] *)
(*s: function [[Repository.read_tree]] *)
let read_tree r h =
  match read_obj r h with
  | Objects.Tree x -> x
  | _ -> failwith "read_tree: was expecting a tree"
(*e: function [[Repository.read_tree]] *)
(*s: function [[Repository.read_blob]] *)
let read_blob r h =
  match read_obj r h with
  | Objects.Blob x -> x
  | _ -> failwith "read_blob: was expecting a blob"
(*e: function [[Repository.read_blob]] *)

(*s: function [[Repository.read_objectish]] *)
let rec read_objectish r objectish =
  match objectish with
  | ObjByRef aref -> 
    (match follow_ref r aref |> snd with
    | None -> failwith (spf "could not resolve %s" (Refs.string_of_ref aref))
    | Some sha -> 
      sha, read_obj r sha
    )
  | ObjByHex hexsha ->
    let sha = Hexsha.to_sha hexsha in
    sha, read_obj r sha
  | ObjByBranch str ->
    read_objectish r (ObjByRef (Refs.Ref ("refs/heads/" ^ str)))
(*e: function [[Repository.read_objectish]] *)

(*s: function [[Repository.add_obj]] *)
let add_obj r obj =
  let bytes = 
    IO.output_bytes () |> IO_.with_close_out (Objects.write obj) in
  let sha = Sha1.sha1 (Bytes.to_string bytes) in
  let hexsha = Hexsha.of_sha sha in
  let file = hexsha_to_filename r hexsha in
  (*s: [[Repository.add_obj()]] create directory if it does not exist *)
  let dir = Filename.dirname file in
  if not (Sys.file_exists dir)
  then Unix.mkdir dir dirperm;
  (*e: [[Repository.add_obj()]] create directory if it does not exist *)
  if (Sys.file_exists file)
  then sha (* deduplication! nothing to write, can share objects *)
  else begin
    file |> with_file_out_with_lock (fun ch ->
      let ic = IO.input_bytes bytes in
      let oc = IO.output_channel ch in
      Compression.compress ic oc;
      IO.close_out oc;
    );
    sha
  end
(*e: function [[Repository.add_obj]] *)

(*s: function [[Repository.has_obj]] *)
let has_obj r h =
  let path = h |> Hexsha.of_sha |> hexsha_to_filename r in
  Sys.file_exists path
(*e: function [[Repository.has_obj]] *)

(*****************************************************************************)
(* Index *)
(*****************************************************************************)

(*s: function [[Repository.read_index]] *)
let read_index r =
  r.index
(*e: function [[Repository.read_index]] *)

(*s: function [[Repository.write_index]] *)
let write_index r =
  let path = index_to_filename r in
  path |> with_file_out_with_lock (fun ch ->
    ch |> IO.output_channel |> IO_.with_close_out (Index.write r.index)
  )
(*e: function [[Repository.write_index]] *)

    
(*s: function [[Repository.content_from_path_and_unix_stat]] *)
let content_from_path_and_unix_stat full_path stat =
  match stat.Unix.st_kind with
  (*s: [[Repository.content_from_path_and_unix_stat()]] match kind cases *)
  | Unix.S_LNK ->
    Unix.readlink full_path
  (*e: [[Repository.content_from_path_and_unix_stat()]] match kind cases *)
  | Unix.S_REG ->
    let full_path = Fpath.v full_path in
    full_path |> UChan.with_open_in (fun (ch : Chan.i) ->
      ch.ic |> IO.input_channel |> IO.read_all
    )
  | _ -> failwith (spf "Repository.add_in_index: %s kind not handled" 
                     full_path)
(*e: function [[Repository.content_from_path_and_unix_stat]] *)

(*s: function [[Repository.add_in_index]] *)
let add_in_index r relpaths =
  (*s: [[Repository.add_in_index()]] sanity check [[relpaths]] *)
  assert (relpaths |> List.for_all Filename.is_relative);
  (*e: [[Repository.add_in_index()]] sanity check [[relpaths]] *)
  relpaths |> List.iter (fun relpath ->
    (*s: [[Repository.add_in_index()]] adding [[relpath]] *)
    let full_path = r.worktree / relpath in
    let stat = 
      try Unix.lstat full_path 
      with Unix.Unix_error _ ->
        failwith (spf "Repository.add_in_index: %s does not exist anymore"
                    relpath)
    in
    let blob = Objects.Blob (content_from_path_and_unix_stat full_path stat) in
    let sha = add_obj r blob in
    let entry = Index.mk_entry relpath sha stat in
    r.index <- Index.add_entry r.index entry;
    (*e: [[Repository.add_in_index()]] adding [[relpath]] *)
  );
  write_index r
(*e: function [[Repository.add_in_index]] *)

(*****************************************************************************)
(* Commit *)
(*****************************************************************************)

(* less: move to cmd_commit.ml? *)
(*s: function [[Repository.commit_index]] *)
let commit_index r author committer message =
  let aref = Refs.Head in
  let root_tree = Index.trees_of_index r.index 
    (fun t -> add_obj r (Objects.Tree t)) 
  in
  (* todo: execute pre-commit hook *)
  (*s: [[Repository.commit_index()]] read merge message if needed *)
  (* less: Try to read commit message from .git/MERGE_MSG *)
  (*e: [[Repository.commit_index()]] read merge message if needed *)
  (* todo: execute commit-msg hook *)
  let commit = { Commit. parents = []; tree = root_tree; 
                 author = author; committer = committer; message = message } in

  let ok =
    match follow_ref r aref |> snd with
    | None ->
      (* first commit so refs/heads/master does not even exist yet *)
      let sha = add_obj r (Objects.Commit commit) in
      (*s: [[Repository.commit_index()]] add ref when first commit *)
      add_ref_if_new r aref (Refs.Hash sha)
      (*e: [[Repository.commit_index()]] add ref when first commit *)
    | Some old_head ->
      (*s: [[Repository.commit_index()]] set [[merge_heads]] *)
      (* less: merge_heads from .git/MERGE_HEADS *)
      let merge_heads = [] in
      (*e: [[Repository.commit_index()]] set [[merge_heads]] *)
      let commit = { commit with Commit.parents = old_head :: merge_heads } in
      let sha = add_obj r (Objects.Commit commit) in
      (*s: [[Repository.commit_index()]] update ref when not first commit *)
      set_ref_if_same_old r aref old_head sha
      (*e: [[Repository.commit_index()]] update ref when not first commit *)
  in
  if not ok
  then failwith (spf "%s changed during commit" (Refs.string_of_ref aref));
  (* todo: execute post-commit hook *)
  ()
(*e: function [[Repository.commit_index]] *)
  
(*****************************************************************************)
(* Checkout and reset *)
(*****************************************************************************)

(*s: function [[Repository.build_file_from_blob]] *)
let build_file_from_blob fullpath blob perm =
  let oldstat =
    try 
      Some (Unix.lstat fullpath)
    with Unix.Unix_error _ -> None
  in
  (match perm with 
  | Tree.Link -> 
    if oldstat <> None
    then Unix.unlink fullpath;
    Unix.symlink blob fullpath;
  | Tree.Normal | Tree.Exec ->
    (match oldstat with
    (* opti: if same content, no need to write anything *)
    | Some { Unix.st_size = x; _ } when x = String.length blob && 
      ((Fpath.v fullpath) |> UChan.with_open_in (fun (ch : Chan.i) -> 
        (ch.ic |> IO.input_channel |> IO.read_all ) = blob
       )) ->
      ()
    | _ ->
      (Fpath.v fullpath) |> UChan.with_open_out (fun (ch : Chan.o) ->
        output_bytes ch.oc (Bytes.of_string blob)
      );
      (* less: honor filemode? *)
      Unix.chmod fullpath 
        (match perm with 
        | Tree.Normal -> 0o644
        | Tree.Exec -> 0o755
        | _ -> raise (Impossible "matched before")
        )
    )
  | Tree.Dir -> raise (Impossible "dirs filtered in walk_tree iteration")
  (*s: [[Repository.build_file_from_blob()]] match perm cases *)
  | Tree.Commit -> failwith "submodule not yet supported"
  (*e: [[Repository.build_file_from_blob()]] match perm cases *)
  );
  Unix.lstat fullpath
(*e: function [[Repository.build_file_from_blob]] *)


(*s: function [[Repository.set_worktree_and_index_to_tree]] *)
let set_worktree_and_index_to_tree r tree =
  (* todo: need lock on index? on worktree? *)
  let hcurrent = 
    r.index |> List.map (fun e -> e.Index.path, false) |> Hashtbl_.of_list in
  let new_index = ref [] in
  (* less: honor file mode from config file? *)
  tree |> Tree.walk_tree (read_tree r) "" (fun relpath entry ->
    let perm = entry.Tree.perm in
    match perm with
    | Tree.Dir -> 
      (* bugfix: need also here to mkdir; doing it below is not enough
       * when a dir has no file but only subdirs
       *)
      let fullpath = r.worktree / relpath in
      if not (Sys.file_exists fullpath)
      then Unix.mkdir fullpath dirperm;
    | Tree.Normal | Tree.Exec | Tree.Link ->
      (* less: validate_path? *)
      let fullpath = r.worktree / relpath in
      if not (Sys.file_exists (Filename.dirname fullpath))
      then Unix.mkdir (Filename.dirname fullpath) dirperm;
      let sha = entry.Tree.id in
      let blob = read_blob r sha in
      let stat = build_file_from_blob fullpath blob perm in
      Hashtbl.replace hcurrent relpath true;
      Stack_.push (Index.mk_entry relpath sha stat) new_index;
    (*s: [[Repository.set_worktree_and_index_to_tree()]] walk tree cases *)
    | Tree.Commit -> failwith "submodule not yet supported"
    (*e: [[Repository.set_worktree_and_index_to_tree()]] walk tree cases *)
  );
  let index = List.rev !new_index in
  r.index <- index;
  write_index r;
  hcurrent |> Hashtbl.iter (fun file used ->
    if not used
    then 
      (* todo: should check if modified? otherwise lose modif! *)
      let fullpath = r.worktree / file in
      Unix.unlink fullpath
  )
  (* less: delete if a dir became empty, just walk_dir? *)
(*e: function [[Repository.set_worktree_and_index_to_tree]] *)

(*****************************************************************************)
(* Packs *)
(*****************************************************************************)

(*****************************************************************************)
(* Repo init/open *)
(*****************************************************************************)

(*s: function [[Repository.init]] *)
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
  (*s: [[Repository.init()]] create [[.git/HEAD]] *)
  let r = {
    worktree = root;
    dotgit = root / ".git";
    index = Index.empty;
  } in
  add_ref_if_new r Refs.Head Refs.default_head_content |> ignore;
  (*e: [[Repository.init()]] create [[.git/HEAD]] *)

  (* less: config file, description, hooks, etc *)
  Sys.chdir root;
  let absolute = Sys.getcwd () in
  UConsole.print (spf "Initialized empty Git repository in %s" (absolute / ".git/"))
(*e: function [[Repository.init]] *)

(*s: function [[Repository.open_]] *)
let open_ root = 
  let path = root / ".git" in
  if Sys.file_exists path &&
     (Unix.stat path).Unix.st_kind = Unix.S_DIR
  then 
    { worktree = root;
      dotgit = path;
      (*s: [[Repository.open_()]] other fields settings *)
      index = 
        (if Sys.file_exists (path / "index")
         then 
          (Fpath.v (path / "index")) |> UChan.with_open_in (fun (ch : Chan.i) ->
            ch.ic |> IO.input_channel |> Index.read)
         else Index.empty
        );
      (*x: [[Repository.open_()]] other fields settings *)
      (* less: grafts, hooks *)
      (*e: [[Repository.open_()]] other fields settings *)
    }
  else failwith (spf "Not a git repository at %s" root)
(*e: function [[Repository.open_]] *)

(*s: function [[Repository.find_dotgit_root_and_open]] *)
let find_root_open_and_adjust_paths paths = 
  (* todo: allow git from different location *)
  let r = open_ "." in
  (* todo: support also absolute paths and transform in relpaths *)
  let relpaths = paths |> List.map (fun path ->
    if Filename.is_relative path
    then 
      (* todo: may have to adjust if root was not pwd *)
      path
    else failwith (spf "TODO: Not a relative path: %s" path)
    )
  in
  r, relpaths
(*e: function [[Repository.find_dotgit_root_and_open]] *)

let parse_objectish _str = 
  raise Todo

(*e: version_control/repository.ml *)
