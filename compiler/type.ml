(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

type blockid = int
type fullname = string * blockid

(* Note that there is no Typedef below. The typechecker
 * expands typedefs. There is no Enum either, because
 * variables using enum (as in 'enum Foo x;') gets their type
 * expanded to an integer type.
 *
 * less: put qualifier here?
 * todoext: Bool! with strict bool checking. 
 * todoext?: Enum of fullname with stricter checking.
 *)
type t =
  | Void
  | I of integer_type
  | F of float_type
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

  and float_type = 
  | Float
  | Double

 and sign = Signed | Unsigned
 and struct_kind = Struct | Union

 (* with tarzan *)


(* Does 5c use this information? Volatile at least? *)
type qualifier = 
  | Volatile
  (* used? seems not really supported *)
  | Const
  (* less: unsupported: | Restrict | Inline *)
 (* with tarzan *)

(* todo: bitfield *)
type structdef = (string * t) list
 (* with tarzan *)

let int = I (Int (Signed))
