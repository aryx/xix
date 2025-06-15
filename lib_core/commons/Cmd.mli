(* Build and run "commands" *)

(* command name, e.g. "git" *)
type name = Name of string [@@deriving show]

(* command arguments *)
type args = string list [@@deriving show]

(* the whole command *)
type t = name * args [@@deriving show]

(* environment variables *)
type env = { vars : (string * string) list; inherit_parent_env : bool }

(* [env_of_list vars] will generate an environment to pass to the
 * CapExec.xxx_of_run functions to execute an external program with
 * environment variables set as desctibed in [vars]. Note that
 * by default inherit_parent_env is set to true meaning the environment
 * variables specified in [vars] are added to the environment of the
 * parent process (and possibly overriding them).
 *)
val env_of_list : (*?inherit_parent_env:*)bool -> (string * string) list -> env

(* for error messages *)
(* val to_string : t -> string *)
