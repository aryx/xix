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
  | Void
  (* integers *)
  | I of integer_type
  (* floats *)
  | F of float_type
  (* Composite *)
  | Pointer of t
  (* Why not unsugar to Pointer? for better error messages! and because
   * the type system checks for array incompatibilities. int[2] != int[3].
   *)
  | Array of int option * t
  | Func of t * t list * bool (* varargs '...' *)
  | StructName of struct_kind * fullname

  and integer_type =
  | Char of sign
  | Short of sign

  | Int of sign
  | Long of sign
  | VLong of sign
  (* less: TEnum of fullname? so stricter! or of t? any, almost never write
   * enum X foo; always abuse int;
   *)

  and float_type = 
  | Float
  | Double

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
