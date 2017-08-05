(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Graph walkers *)
(*****************************************************************************)

(* will start from the heads and iterate over the ancestry of heads
 * until the caller ack that some top commits are already known and
 * do not need to be iterated furthermore.
 *)
type graph_walker = {
  next: unit -> Commit.hash option;
  ack: Commit.hash -> unit;
}

let mk_graph_walker r =
  (* less: start just from HEAD? *)
  let heads = 
    Repository.all_refs r |> Common.map_filter (fun aref ->
      if aref =~ "refs/heads/"
      then Some (Repository.follow_ref_some r (Refs.Ref aref))
      else None
    )
  in
  let todos = ref heads in
  (* acts as an 'hdone' too *)
  let hparents = Hashtbl.create 101 in
  { next = (fun () ->
    match !todos with
    | [] -> None
    | x::xs ->
      todos := xs;
      let commit = Repository.read_commit r x in
      let parents = commit.Commit.parents in
      Hashtbl.add hparents x parents;
      parents |> List.iter (fun parent ->
        if Hashtbl.mem hparents parent
        then ()
        else todos := parent::!todos
      );
      Some x
    );
    ack = (fun commit_sha ->
      (* less: do weird loop where recurse also over parents as in dulwich? *)
      raise Todo
    );
  }


let collect_ancestors commits hdone =
  raise Todo

let collect_filetree read_tree tree have_sha =
  raise Todo
    
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
 
(* find the common frontline *)
let find_top_common_commits src dst =
  let top_commons = Hashtbl.create 101 in
  let walker = mk_graph_walker dst in

  let rec loop_while_sha commit_sha_opt =
    commit_sha_opt |> Common.if_some (fun commit_sha ->
      if Repository.has_obj src commit_sha
      then begin
        Hashtbl.add top_commons commit_sha true;
        walker.ack commit_sha;
      end;
      loop_while_sha (walker.next ())
    )
  in
  loop_while_sha (walker.next ());
  top_commons


let iter_missing_objects top_common_commits top_wanted_commits src f =
  (* less: split_commits_and_tags? *)
  let all_common_commits = 
    collect_ancestors top_common_commits (Hashtbl.create 101) in
  let missing_commits = 
    collect_ancestors top_wanted_commits all_common_commits in

  (* let's iterate over all common commits *)
  
  let dst_have_sha = Hashtbl.create 101 in
  (* less: start from second returned result from collect_ancestors?
   * common_commits different from all_ancestors in VCS.nw? 
   *)
  (* expensive loop below? so use parallel threads? *)
  all_common_commits |> List.iter (fun commit_sha ->
    Hashtbl.add dst_have_sha commit_sha true;
    let commit = Repository.read_commit src commit_sha in
    collect_filetree (Repository.read_tree src) commit.Commit.tree dst_have_sha
  );

  (* and now let's iterate over all missing commits *)

  (* less: tags handling *)
  let rec aux sha is_blob = 
    if Hashtbl.mem dst_have_sha sha
    then ()
    else begin
      Hashtbl.add dst_have_sha sha true;
      (if is_blob
       then f sha None
       else begin
        let obj = Repository.read_obj src sha in
        f sha (Some obj);
        (match obj with
        | Objects.Commit commit ->
          aux commit.tree false
        | Objects.Tree tree ->
          tree |> List.iter (fun entry ->
            if entry.Tree.perm = Tree.Commit
            then failwith "submodule not supported";
            aux entry.Tree.node (entry.Tree.perm = Tree.Dir)
          )
        | Objects.Blob _ ->
          raise (Impossible "is_blob guard")
        )
       end
      );
    end
  in
  missing_commits |> Hashtbl.iter (fun commit_sha _true ->
    aux commit_sha false
  )

  
  

let fetch_objects src dst =
  (* less: determine_wants from pull command *)
  let top_wanted_commits = [Repository.follow_ref_some src Refs.Head] in
  (* less: shallows? unshallows? *)
  let top_common_commits = find_top_common_commits src dst in
  iter_missing_objects top_common_commits top_wanted_commits src 
    (fun sha1 obj_opt ->
    (* less: opti: copy raw files directly without unmarshalling/marshalling *)
    let obj = 
      match obj_opt with
      | None -> Repository.read_obj src sha1
      | Some obj -> obj
    in
    let sha2 = Repository.add_obj dst obj in
    assert (sha1 = sha2)
  )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let mk_client path =
  { Client.
    url = path;
    fetch = (fun dst ->
      let src = Repository.open_ path in
      fetch_objects src dst;
      (* less: all_refs *)
      Repository.follow_ref_some src Refs.Head
   );
  }
