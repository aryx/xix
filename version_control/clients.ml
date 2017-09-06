(*s: version_control/clients.ml *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function Clients.client_of_url *)
(* old: was called get_transport_and_path (and xxx_from_url) in dulwich *)
let client_of_url url =
  match url with
  (* less: should use URL parsing library *)
  | s when s =~ "^git://" -> 
    Client_git.mk_client url
  | s when s =~ "^ssh://" -> 
    failwith "ssh not supported"
  | s when s =~ "^http://" -> 
    failwith "http not supported"
  | s -> 
    if Sys.file_exists s
    then Client_local.mk_client url
    else failwith (spf "remote repository URL not supported: %s" url)
(*e: function Clients.client_of_url *)
(*e: version_control/clients.ml *)
