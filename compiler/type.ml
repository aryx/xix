(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(* Same than in ast.ml, but repeated here to avoid a mutual dependency *)
type blockid = int
type fullname = string * blockid

type sign = Signed | Unsigned

(* Note that there is no TTypedef here; 
 * The typedef expansion has already been done.
 * less: put qualifier here?
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

  | TEnum (* of fullname? *)

  (* floats *)
  | TFloat
  | TDouble

  (* Composite *)

  | TPointer of t
  (* Why not unsugar to TIndirect? for better error messages? *)
  | TArray of t (* no size here *)

  | TFunc of t * t list

  (* less: could merge in TStruct of struct_kind *)
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
  | Enum (* less: of intsize? *)
 (* with tarzan *)
