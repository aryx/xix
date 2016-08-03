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
}

let internal_vars = [
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
  let env = 
    { vars          = Hashtbl.create 101;
      internal_vars = Hashtbl.create 101;
    }
  in
  Shellenv.read_environment () |> List.iter (fun (s, xs) ->
    (* todo: checks? *)
    Hashtbl.add env.vars s xs
  );
  env
