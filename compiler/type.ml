(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

type blockid = int
type fullname = string * blockid

type sign = Signed | Unsigned

type struct_kind = Struct | Union

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

  (* less: of fullname? so stricter! or of t? any, almost never write
   * enum X foo; always abuse int;
   *)
  | TEnum 

  (* floats *)
  | TFloat
  | TDouble

  (* Composite *)

  | TPointer of t
  (* Why not unsugar to TIndirect? for better error messages? *)
  | TArray of t (* no size here *)

  | TFunc of t * t list * bool (* varargs '...' *)

  | TStructName of struct_kind * fullname
 (* with tarzan *)


(* Does 5c use this information? Volatile at least? *)
type qualifier = 
  | Volatile
  (* used? seems not really supported *)
  | Const
  (* less: unsupported: | Restrict | Inline *)
 (* with tarzan *)

type structdef = (string * t) list
type enumdef = Enum (* less: of intsize? or float! *)

 (* with tarzan *)
