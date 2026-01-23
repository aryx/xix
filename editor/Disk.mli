(*s: Disk.mli *)
(*s: signature [[Disk.putline]] *)
(* store line (with added trailing '\n') in tfile and return its offset *)
val putline : Env.t -> string -> Env.tfile_offset
(*e: signature [[Disk.putline]] *)
(*s: signature [[Disk.getline]] *)
(* retrieve line in tfile (without trailing '\n') *)
val getline: Env.t -> Env.lineno -> string
(*e: signature [[Disk.getline]] *)

(*s: signature [[Disk.getfile]] *)
(* will return one line (without trailing '\n') or None when reached EOF *)
val getfile: Env.t -> Chan.i -> (unit -> string option)
(*e: signature [[Disk.getfile]] *)
(*s: signature [[Disk.putfile]] *)
(* dual of getfile() but this time writing all the lines, not just one *)
val putfile: Env.t -> Chan.o -> unit
(*e: signature [[Disk.putfile]] *)
(*e: Disk.mli *)
