(*s: version_control/client_local.ml *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Graph walkers *)
(*****************************************************************************)

(*s: type Client_local.graph_walker *)
(* will start from the heads and iterate over the ancestry of heads
 * until the caller ack that some top commits are already known and
 * do not need to be iterated furthermore.
 *)
type graph_walker = {
  next: unit -> Commit.hash option;
  ack: Commit.hash -> unit;
}
(*e: type Client_local.graph_walker *)

(*s: function Client_local.ml_graph_walker *)
let (mk_graph_walker: Repository.t -> graph_walker) = fun r ->
  (* less: start just from HEAD? *)
  let heads = 
    Repository.all_refs r |> Common.map_filter (fun aref ->
      if aref =~ "refs/heads/"
      then Some (Repository.follow_ref_some r (Refs.Ref aref))
      else None
    )
  in
  let todos = ref heads in
  let todos_next_round = ref [] in
  let last_round = ref None in
  let hdone = Hashtbl.create 101 in
  { next = (fun () ->
    todos := !todos_next_round @ !todos;
    todos_next_round := [];
    match !todos with
    | [] -> None
    | x::xs ->
      todos := xs;
      Hashtbl.add hdone x true;
      last_round := Some x;
      let commit = Repository.read_commit r x in
      let parents = commit.Commit.parents in
      parents |> List.iter (fun parent ->
        if Hashtbl.mem hdone parent
        then ()
        else todos_next_round := parent::!todos_next_round;
      );
      Some x
    );
    ack = (fun commit_sha ->
      (* less: do weird loop where recurse also over parents as in dulwich? *)
      match !last_round with
      | None -> raise (Impossible "ack always after at least one next");
      | Some x ->
        if x <> commit_sha
        then raise (Impossible "'ack(x)' should follow 'x = next()'");
        (* skip those one then because parent already in common *)
        todos_next_round := []
    );
  }
(*e: function Client_local.ml_graph_walker *)

(*s: function Client_local.collect_filetree *)
let rec collect_filetree read_tree treeid have_sha =
  let tree = read_tree treeid in
  tree |> List.iter (fun entry ->
    let sha = entry.Tree.node in
    if not (Hashtbl.mem have_sha sha) then begin
      Hashtbl.add have_sha sha true;
      match entry.Tree.perm with
      | Tree.Normal | Tree.Exec | Tree.Link -> ()
      | Tree.Dir ->  collect_filetree read_tree sha have_sha
      | Tree.Commit -> failwith "submodule not supported yet"
    end
  )
(*e: function Client_local.collect_filetree *)
    
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
 
(*s: function Client_local.find_top_common_commits *)
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
  top_commons |> Hashtbl_.to_list |> List.map fst
(*e: function Client_local.find_top_common_commits *)


(*s: function Client_local.iter_missing_objects *)
let iter_missing_objects top_common_commits top_wanted_commits src f =
  (* less: split_commits_and_tags? *)
  let all_common_commits = 
    Commit.collect_ancestors (Repository.read_commit src) top_common_commits 
      (Hashtbl.create 101) in
  (* bugfix: do not forget Hashtbl.copy because collect_ancestors modify 
   * the second parameter by side effect
   *)
  let missing_commits = 
    Commit.collect_ancestors (Repository.read_commit src) top_wanted_commits 
      (Hashtbl.copy all_common_commits)
  in

  (* let's iterate over all common commits *)
  
  let dst_have_sha = Hashtbl.create 101 in
  (* less: start from second returned result from collect_ancestors?
   * common_commits different from all_ancestors in VCS.nw? 
   *)
  (* expensive loop below? so use parallel threads? *)
  all_common_commits |> Hashtbl.iter (fun commit_sha _true ->
    Hashtbl.add dst_have_sha commit_sha true;
    let commit = Repository.read_commit src commit_sha in
    collect_filetree (Repository.read_tree src) commit.Commit.tree dst_have_sha
  );

  (* and now let's iterate over all missing commits *)

  (* less: tags handling *)
  let rec missing sha is_blob = 
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
          missing commit.Commit.tree false
        | Objects.Tree tree ->
          tree |> List.iter (fun entry ->
            if entry.Tree.perm = Tree.Commit
            then failwith "submodule not supported";
            (* bugfix: it's <>, not = *)
            missing entry.Tree.node (entry.Tree.perm <> Tree.Dir)
          )
        | Objects.Blob _ ->
          raise (Impossible "is_blob guard")
        )
       end
      );
    end
  in
  missing_commits |> Hashtbl.iter (fun commit_sha _true ->
    missing commit_sha false
  )
(*e: function Client_local.iter_missing_objects *)

  
  

(*s: function Client_local.fetch_objects *)
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
    (* todo: count objects progress *)
    (* pr2 (spf "adding %s" (Hexsha.of_sha sha1)); *)
    let sha2 = Repository.add_obj dst obj in
    assert (sha1 = sha2)
  )
(*e: function Client_local.fetch_objects *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function Client_local.mk_client *)
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
(*e: function Client_local.mk_client *)
(*e: version_control/client_local.ml *)
