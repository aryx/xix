
val setup : 
  Logs.level option -> unit -> unit

(* the -verbose/-debug/-quiet CLI flags *)
val cli_flags : 
  Logs.level option ref -> (Arg.key * Arg.spec * Arg.doc) list
