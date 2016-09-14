
type sign = Signed | Unsigned

(* Note that no TTypedef here. The typedef expansion has been done. *)
type t =
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

  (* composite *)
  | TIndirect of type_
  (* Why not unsugar to TIndirect? for better error messages? *)
  | TArray of type_ (* no size here *)

  | TFunc of type_ * (type_) list

  (* less: and scope? counter?
   * ref to symbol? or use external hash?
   *)
  | TStructName of string
  | TUnionName of string


(* less: type qualifier, but 5c does not use this information *)

type tagdef =
  | Struct of (string * t) list;
  | Union of (string * t) list;
  | Enum
