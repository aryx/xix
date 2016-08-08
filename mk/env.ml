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
(* Debug *)
(*****************************************************************************)
let dump_env env =
  env.vars |> Hashtbl.iter (fun k v ->
    pr2 (spf "%s -> %s" k (Common.dump v));
  )


(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

(* less: could take the readenv function as a parameter? *)
let initenv () =
  let internal = 
    mk_vars |> List.map (fun k -> k,[]) |> Common.hash_of_list in
  let vars = 
    Shellenv.read_environment () |> Common.exclude (fun (s, _) ->
      (* when you use mk recursively, the environment might contain
       * a $stem from a parent mk process.
       *)
      Hashtbl.mem internal s
    ) |> Common.hash_of_list
  in

  (* less: extra checks and filtering on read_environment? *)
  { vars          = vars;
    internal_vars = internal;
    vars_we_set   = Hashtbl.create 101;
  }

let shellenv_of_env env =
  Common.hash_to_list env.internal_vars @
  Common.hash_to_list env.vars
