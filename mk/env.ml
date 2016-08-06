(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* content of variables (after full expansion and backquote resolution) *)
type values = string list

type t = {
  vars         : (string, values) Hashtbl.t;
  internal_vars: (string, values) Hashtbl.t;

  vars_we_set: (string, bool) Hashtbl.t;
}

let mk_vars = [
  "target";
  "prereq";
  "stem";

  (* todo: alltargets, newprereq ... 
  *)
]

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

(* less: could take the readenv function as a parameter? *)
let initenv () =
  (* less: extra checks and filtering on read_environment? *)
  { vars          = Shellenv.read_environment () |> Common.hash_of_list;
    internal_vars = mk_vars |> List.map (fun k -> k,[]) |> Common.hash_of_list;
    vars_we_set   = Hashtbl.create 101;
  }

let shellenv_of_env env =
  Common.hash_to_list env.internal_vars @
  Common.hash_to_list env.vars
