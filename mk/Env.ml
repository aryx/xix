(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Content of variables (after full expansion and backquote resolution).
 * It should not contain any empty strings (but it can contain empty lists).
 *)
type values = string list

type t = {
  (* use Env.add_var to add a var (to check if it's ok) *)
  vars         : (string, values) Hashtbl.t;
  internal_vars: (string, values) Hashtbl.t;

  (* those vars can not be overriden by the mkfile *)
  vars_commandline: (string, bool) Hashtbl.t;
  vars_we_set: (string, bool) Hashtbl.t;
}

let mk_vars = [
  "target";
  "prereq";
  "stem";

  (* todo: alltargets, newprereq ... 
  *)
]

(* invariant *)
let check_values xs = 
  xs |> List.iter (fun s ->
    if s = ""
    then raise (Impossible (spf "empty string in values"))
  )

exception Redefinition of string
let add_var env s xs = 
  match () with
  | _ when Hashtbl.mem env.vars_commandline s ->
    (* we do not override those vars *)
    Logs.info (fun m -> m "ignoring definition of %s specified on the command-line" s);
    ()

  (* stricter: forbid redefinitions.
   * (bug: but ok to redefine variable from environment, otherwise
   *  hard to use mk recursively, hence the use of vars_we_set below)
   * less: could allow to redefine in strict mode if previous
   *  def was empty.
   *)
  | _ when Hashtbl.mem env.vars s && Hashtbl.mem env.vars_we_set s 
      && !Flags.strict_mode ->
    raise (Redefinition s)
  | _ ->
    Hashtbl.replace env.vars s xs

(*****************************************************************************)
(* Debug *)
(*****************************************************************************)
let dump_env env =
  Logs.debug (fun m -> m "Dump_env:");
  env.vars |> Hashtbl.iter (fun k v ->
    Logs.debug (fun m -> m " %s -> %s" k (Dumper.dump v));
  )

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

(* less: could take the readenv function as a parameter? *)
let initenv (caps : < Cap.env; Cap.argv; .. >) =
  let internal = 
    mk_vars |> List.map (fun k -> k,[]) |> Hashtbl_.of_list in
  let vars = 
    Shellenv.read_environment caps |> List_.exclude (fun (s, _) ->
      (* when you use mk recursively, the environment might contain
       * a $stem from a parent mk process.
       *)
      Hashtbl.mem internal s
    ) |> Hashtbl_.of_list
  in

  (* for recursive mk *)
  let mkflags = 
    CapSys.argv caps |> Array.fold_left (fun acc s ->
      if s =~ "^-" || s=~ ".*=.*"
      then s::acc
      else acc
    ) []
  in
  Hashtbl.add vars "MKFLAGS" (List.rev mkflags);

  (* less: extra checks and filtering on read_environment? *)
  { vars          = vars;
    internal_vars = internal;
    vars_we_set   = Hashtbl.create 101;
    vars_commandline   = Hashtbl.create 101;
  }

let shellenv_of_env env =
  Hashtbl_.to_list env.internal_vars @
  Hashtbl_.to_list env.vars
