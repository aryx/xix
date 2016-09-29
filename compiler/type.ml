
type sign = Signed | Unsigned

(* Note that there is no TTypedef here; The typedef expansion
 * has alreafy been done. 
 *)
type t =
  (* Basic *)

  | TVoid

  (* integers *)
  | TChar of sign
  | TShort of sign

  | TInt of sign
  | TLong of sign
  | TVLong of sign

  | TEnum (* of string? *)

  (* floats *)
  | TFloat
  | TDouble

  (* Composite *)

  | TPointer of t
  (* Why not unsugar to TIndirect? for better error messages? *)
  | TArray of t (* no size here *)

  | TFunc of t * t list

  (* less: and scope? counter?
   * ref to symbol? or use external hash?
   *)
  | TStructName of string
  | TUnionName of string


(* 5c does not use this information? for volatile at least? *)
type qualifier = 
  | Volatile
  (* used? seems not really supported *)
  | Const
  (* less: unsupported: | Restrict | Inline *)

type tagdef =
  | Struct of (string * t) list
  | Union of (string * t) list
  | Enum
