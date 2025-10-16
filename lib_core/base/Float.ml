(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external neg : float -> float = "%negfloat"
external add : float -> float -> float = "%addfloat"
external sub : float -> float -> float = "%subfloat"
external mul : float -> float -> float = "%mulfloat"
external div : float -> float -> float = "%divfloat"
(* external rem : float -> float -> float = "caml_fmod_float" "fmod" 
 (*[@@unboxed] [@@noalloc]*) *)
external abs : float -> float = "%absfloat"
(*
let infinity = Pervasives.infinity
let neg_infinity = Pervasives.neg_infinity
let nan = Pervasives.nan
let max_float = Pervasives.max_float
let min_float = Pervasives.min_float
let epsilon_float = Pervasives.epsilon_float
*)

external of_int : int -> float = "%floatofint"
external to_int : float -> int = "%intoffloat"
external of_string : string -> float = "float_of_string"
(*let of_string_opt = Pervasives.float_of_string_opt *)
let to_string = (*Pervasives.*)string_of_float

(*
type fpclass = Pervasives.fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float : (float (*[@unboxed]*)) -> fpclass =
  "caml_classify_float" "caml_classify_float_unboxed" (*[@@noalloc]*)
 *)

external pow : float -> float -> float = "power_float" "pow"
  (*[@@unboxed] [@@noalloc]*)
external sqrt : float -> float = "sqrt_float" "sqrt"
  (*[@@unboxed] [@@noalloc]*)
external exp : float -> float = "exp_float" "exp" (*[@@unboxed] [@@noalloc]*)
external log : float -> float = "log_float" "log" (*[@@unboxed] [@@noalloc]*)
external log10 : float -> float = "log10_float" "log10"
  (*[@@unboxed] [@@noalloc]*)
(*external expm1 : float -> float = "expm1_float" "caml_expm1"
 (*[@@unboxed] [@@noalloc]*) *)
(*external log1p : float -> float = "log1p_float" "caml_log1p"
  (*[@@unboxed] [@@noalloc]*)
 *)
external cos : float -> float = "cos_float" "cos" (*[@@unboxed] [@@noalloc]*)
external sin : float -> float = "sin_float" "sin" (*[@@unboxed] [@@noalloc]*)
external tan : float -> float = "tan_float" "tan" (*[@@unboxed] [@@noalloc]*)
external acos : float -> float = "acos_float" "acos"
  (*[@@unboxed] [@@noalloc]*)
external asin : float -> float = "asin_float" "asin"
  (*[@@unboxed] [@@noalloc]*)
external atan : float -> float = "atan_float" "atan"
  (*[@@unboxed] [@@noalloc]*)
external atan2 : float -> float -> float = "atan2_float" "atan2"
  (*[@@unboxed] [@@noalloc]*)
(*external hypot : float -> float -> float
               = "hypot_float" "caml_hypot" (*[@@unboxed] [@@noalloc]*) *)
external cosh : float -> float = "cosh_float" "cosh"
  (*[@@unboxed] [@@noalloc]*)
external sinh : float -> float = "sinh_float" "sinh"
  (*[@@unboxed] [@@noalloc]*)
external tanh : float -> float = "tanh_float" "tanh"
  (*[@@unboxed] [@@noalloc]*)
external ceil : float -> float = "ceil_float" "ceil"
  (*[@@unboxed] [@@noalloc]*)
external floor : float -> float = "floor_float" "floor"
(*[@@unboxed] [@@noalloc]*)
(*external copysign : float -> float -> float
                  = "caml_copysign_float" "caml_copysign"
 (*[@@unboxed] [@@noalloc]*) *)
(*external frexp : float -> float * int = "caml_frexp_float" *)
(* external ldexp : (float (*[@unboxed]*)) -> (int (*[@untagged]*)) -> (float (*[@unboxed]*)) =
  "caml_ldexp_float" "caml_ldexp_float_unboxed" (*[@@noalloc]*)
 *)

(*external modf : float -> float * float = "caml_modf_float" *)
type t = float
(*
external compare : float -> float -> int = "%compare"
 *)
let compare : t -> t -> int = compare
let equal x y = compare x y = 0
