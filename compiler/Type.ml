(*s: Type.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(* TODO? rename to Type_.ml because conflict with OCaml5 module name *)

(*s: type [[Type.blockid]] *)
type blockid = int
(*e: type [[Type.blockid]] *)
[@@deriving show]
(*s: type [[Type.fullname]] *)
type fullname = string * blockid
(*e: type [[Type.fullname]] *)
[@@deriving show]

(*s: type [[Type.t]] *)
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
  (* basic types *)
  | Void
  | I of integer_type
  | F of float_type

  (* composite types *)
  | Pointer of t
  (* Why not unsugar Array to Pointer? Because the type system checks 
   * for some array incompatibilities. int[2] != int[3].
   * However, the Array type usually gets converted to Pointer
   * during typechecking (see array_to_pointer())
   *)
  | Array of int option * t
  | Func of t * t list * bool (* varargs '...' *)
  | StructName of struct_kind * fullname
(*e: type [[Type.t]] *)

(*s: type [[Type.integer_type]] *)
  and integer_type = integer_kind * sign
(*e: type [[Type.integer_type]] *)
(*s: type [[Type.integer_kind]] *)
    and integer_kind = 
    | Char
    | Short
    | Int
    | Long
    | VLong
(*e: type [[Type.integer_kind]] *)
(*s: type [[Type.sign]] *)
    and sign = Signed | Unsigned
(*e: type [[Type.sign]] *)

(*s: type [[Type.float_type]] *)
  and float_type = 
  | Float
  | Double
(*e: type [[Type.float_type]] *)

(*s: type [[Type.struct_kind]] *)
 and struct_kind = Struct | Union
(*e: type [[Type.struct_kind]] *)
[@@deriving show { with_path = false }]

(*s: type [[Type.qualifier]] *)
type qualifier = 
  | Volatile
  | Const
  (* less: unsupported: | Restrict | Inline *)
(*e: type [[Type.qualifier]] *)
[@@deriving show]


(*s: type [[Type.structdef]] *)
(* Note that the field can be gensym'ed for anonymous struct/union elements.
 * Note also structdef is not part of Type.t above; structdef is used
 * instead in Typecheck.typed_program
 * todo: bitfield
 *)
type structdef = (string * t) list
(*e: type [[Type.structdef]] *)
[@@deriving show]

(*s: constant [[Type.int]] *)
let int = I (Int, Signed)
(*e: constant [[Type.int]] *)
(*s: constant [[Type.long]] *)
let long = I (Long, Signed)
(*e: constant [[Type.long]] *)
(*e: Type.ml *)
