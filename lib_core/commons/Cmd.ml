(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Build and run external "commands".
 *
 * This is a capability-aware alternative to Sys.command
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type name = Name of string [@@deriving show]
type args = string list [@@deriving show]
type t = name * args [@@deriving show]

(* alt: we could also make it part of [t] and have a triple *)
type env = { vars : (string * string) list; inherit_parent_env : bool }

let env_of_list (inherit_parent_env : bool) (vars : (string * string) list) :
    env =
  { vars = vars; inherit_parent_env = inherit_parent_env }

(*****************************************************************************)
(* API *)
(*****************************************************************************)
