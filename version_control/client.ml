(*s: version_control/client.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type Client.t *)
type t = {
  (* path to remote (e.g., /path/other/repo, or git://github.com/foo/bar) *)
  url: string;
  (* less: more parameters:
   *  - determine_refs_wanted. for now grabs everything from remote HEAD 
   *  - return set of remote refs, not just the one for HEAD
   * Note that fetch will modify the target repository by side effect.
   *)
  fetch: Repository.t -> Commit.hash;
  (* less: progress *)
}
(*e: type Client.t *)
(*e: version_control/client.ml *)
