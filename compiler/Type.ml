(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
(* renamed to Type_.ml because conflict with OCaml5 module name *)

type blockid = int
type fullname = string * blockid

(* The C type system.
 *
 * There is no Typedef below. The typechecker expands typedefs. 
 * There is no Enum either because variables using enum 
 * (as in 'enum Foo x;') gets their type expanded to an integer type.
 *
 * less: put qualifier here?
 * todoext: Bool! with strict bool checking. 
 * todoext: Enum of fullname with stricter checking.
 *)
type t =
  | Void
  | I of integer_type
  | F of float_type
  | Pointer of t
  (* Why not unsugar Array to Pointer? Because the type system checks 
   * for some array incompatibilities. int[2] != int[3].
   * However, the Array type usually gets converted to Pointer
   * during typechecking.
   *)
  | Array of int option * t
  | Func of t * t list * bool (* varargs '...' *)
  | StructName of struct_kind * fullname

  and integer_type = integer_kind * sign
    and integer_kind = 
    | Char
    | Short
    | Int
    | Long
    | VLong
    and sign = Signed | Unsigned

  and float_type = 
  | Float
  | Double

 and struct_kind = Struct | Union
 (* with tarzan *)

type qualifier = 
  | Volatile
  | Const
  (* less: unsupported: | Restrict | Inline *)
 (* with tarzan *)

(* note that the field can be gensym'ed for anonymous struct/union elements *)
(* todo: bitfield *)
type structdef = (string * t) list
 (* with tarzan *)

let int = I (Int, Signed)
let long = I (Long, Signed)
