(*s: Error.mli *)

(*s: signature [[Error.warn]] *)
val warn: string -> Location_cpp.loc -> unit
(*e: signature [[Error.warn]] *)

(*s: signature [[Error.errorexit]] *)
(* this raises Exit.ExitCode 1 *)
val errorexit: string -> 'a
(*e: signature [[Error.errorexit]] *)
(*e: Error.mli *)
