(*s: Rewrite.ml *)

(* todo mandatory:
 *  - pointer arithmetic
 *  - automatic casts
 * todo for opti?:
 *  - OADDR/OIND simplifications
 *  - put constants on the right for commutative operations
 *)

let rewrite (tast: Typecheck.typed_program) : Typecheck.typed_program =
  tast
(*e: Rewrite.ml *)
