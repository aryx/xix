(*s: Disk.mli *)

(*s: signature [[Disk.putline]] *)
val putline : Env.t -> string -> Env.tfile_offset
(*e: signature [[Disk.putline]] *)
(*s: signature [[Disk.getline]] *)
val getline: Env.t -> Env.lineno -> string
(*e: signature [[Disk.getline]] *)

(*s: signature [[Disk.getfile]] *)
val getfile: Env.t -> Chan.i -> (unit -> string option)
(*e: signature [[Disk.getfile]] *)
(*s: signature [[Disk.putfile]] *)
val putfile: Env.t -> Chan.o -> unit
(*e: signature [[Disk.putfile]] *)
(*e: Disk.mli *)
