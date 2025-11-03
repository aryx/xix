(*s: version_control/clients.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common
open Regexp_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Clients.client_of_url]] *)
(* old: was called get_transport_and_path (and xxx_from_url) in dulwich *)
let client_of_url url =
  match url with
  (*s: [[Clients.client_of_url()]] match url cases *)
  (* less: should use URL parsing library *)
  | s when s =~ "^git://" -> 
    Client_git.mk_client url
  (*x: [[Clients.client_of_url()]] match url cases *)
  | s when s =~ "^ssh://" -> 
    failwith "ssh not supported"
  (*x: [[Clients.client_of_url()]] match url cases *)
  | s when s =~ "^http://" -> 
    failwith "http not supported"
  (*e: [[Clients.client_of_url()]] match url cases *)
  | s -> 
    if Sys.file_exists s
    then Client_local.mk_client (Fpath.v s)
    else failwith (spf "remote repository URL not supported: %s" url)
(*e: function [[Clients.client_of_url]] *)
(*e: version_control/clients.ml *)
