(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let find_common_commits src dst =
  raise Todo

let find_missing_objects commit_haves commit_wants src =
  raise Todo

let fetch_objects src dst =
  (* less: determine_wants from pull command *)
  let commit_wants = [Repository.follow_ref_some src Refs.Head] in
  (* less: shallows? unshallows? *)
  let commit_haves = find_common_commits src dst in
  let missing = find_missing_objects commit_haves commit_wants src in
  missing |> List.iter (fun sha1 ->
    (* less: opti: copy raw files directly without unmarshalling/marshalling *)
    let obj = Repository.read_obj src sha1 in
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
