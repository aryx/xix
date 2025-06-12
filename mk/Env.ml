(*s: mk/Env.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Env.values]] *)
(* Content of variables (after full expansion and backquote resolution).
 * It should not contain any empty strings (but it can contain empty lists).
 *)
type values = string list
(*e: type [[Env.values]] *)

(*s: type [[Env.t]] *)
type t = {
  (* use Env.add_var to add a var (to check if it's ok) *)
  vars         : (string, values) Hashtbl.t;
  (*s: [[Env.t]] fields *)
  internal_vars: (string, values) Hashtbl.t;
  (*x: [[Env.t]] fields *)
  (* those vars can not be overriden by the mkfile *)
  vars_commandline: string Hashtbl_.set;
  (*x: [[Env.t]] fields *)
  vars_we_set: string Hashtbl_.set;
  (*e: [[Env.t]] fields *)
}
(*e: type [[Env.t]] *)

(*s: constant [[Env.mk_vars]] *)
let mk_vars = [
  "target";
  "prereq";
  "stem";

  (* todo: alltargets, newprereq ... *)
]
(*e: constant [[Env.mk_vars]] *)

(*s: function [[Env.check_values]] *)
(* invariant *)
let check_values xs = 
  xs |> List.iter (fun s ->
    if s = ""
    then raise (Impossible (spf "empty string in values"))
  )
(*e: function [[Env.check_values]] *)

(*s: exception [[Env.Redefinition]] *)
exception Redefinition of string
(*e: exception [[Env.Redefinition]] *)
(*s: function [[Env.add_var]] *)
let add_var env s xs = 
  match () with
  (*s: [[Env.add_var()]] ignore def if overriden on the command-line *)
  | _ when Hashtbl.mem env.vars_commandline s ->
    (* we do not override those vars *)
    Logs.info (fun m -> m "ignoring definition of %s specified on the command-line" s);
    ()
  (*e: [[Env.add_var()]] ignore def if overriden on the command-line *)
  (*s: [[Env.add_var()]] forbid redefinition in strict mode *)
  (* stricter: forbid redefinitions.
   * (bug: but ok to redefine variable from environment, otherwise
   *  hard to use mk recursively, hence the use of vars_we_set below)
   * less: could allow to redefine in strict mode if previous
   *  def was empty.
   *)
  | _ when Hashtbl.mem env.vars s && Hashtbl.mem env.vars_we_set s 
      && !Flags.strict_mode ->
    raise (Redefinition s)
  (*e: [[Env.add_var()]] forbid redefinition in strict mode *)
  | _ ->
    Hashtbl.replace env.vars s xs
(*e: function [[Env.add_var]] *)

(*****************************************************************************)
(* Debug *)
(*****************************************************************************)
(*s: function [[Env.dump_env]] *)
let dump_env env =
  Logs.debug (fun m -> m "Dump_env:");
  env.vars |> Hashtbl.iter (fun k v ->
    Logs.debug (fun m -> m " %s -> %s" k (Dumper.dump v));
  )
(*e: function [[Env.dump_env]] *)

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

(*s: function [[Env.initenv]] *)
(* less: could take the readenv function as a parameter? *)
let initenv (caps : < Cap.env; Cap.argv; .. >) =
  let internals = 
    mk_vars |> List.map (fun k -> k,[]) |> Hashtbl_.of_list in
  let vars = 
    Shellenv.read_environment caps |> List_.exclude (fun (s, _) ->
      (* when you use mk recursively, the environment might contain
       * a $stem from a parent mk process.
       *)
      Hashtbl.mem internals s
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
    internal_vars = internals;

    vars_we_set   = Hashtbl.create 101;
    vars_commandline   = Hashtbl.create 101;
  }
(*e: function [[Env.initenv]] *)

(*s: function [[Env.shellenv_of_env]] *)
let shellenv_of_env (env : t) : Shellenv.t =
  Hashtbl_.to_list env.internal_vars @
  Hashtbl_.to_list env.vars
(*e: function [[Env.shellenv_of_env]] *)
(*e: mk/Env.ml *)
