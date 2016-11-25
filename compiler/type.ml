(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(* Same than Ast.blockid, but repeated here to avoid a mutual dependency *)
type blockid = int
 (* with tarzan *)

type fullname = string * blockid

type sign = Signed | Unsigned
 (* with tarzan *)

(* Note that there is no TTypedef here; 
 * The typedef expansion has already been done.
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
   * less: could merge in TStruct of struct_kind
   *)
  | TStructName of fullname
  | TUnionName of fullname
 (* with tarzan *)


(* Does 5c use this information? Volatile at least? *)
type qualifier = 
  | Volatile
  (* used? seems not really supported *)
  | Const
  (* less: unsupported: | Restrict | Inline *)
 (* with tarzan *)

type tagdef =
  | Struct of (string * t) list
  | Union of (string * t) list
  | Enum
 (* with tarzan *)
