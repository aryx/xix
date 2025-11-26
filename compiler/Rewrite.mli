(*s: Rewrite.mli *)
val rewrite: Typecheck.typed_program -> Typecheck.typed_program

type error = Check.error
exception Error of error
val string_of_error: error -> string

(*e: Rewrite.mli *)
