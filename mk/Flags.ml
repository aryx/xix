(*s: mk/Flags.ml *)
(*s: constant [[Flags.dry_mode]] *)
let dry_mode = ref false
(*e: constant [[Flags.dry_mode]] *)

(* TODO? just use Logs.info for those? *)
(*s: constant [[Flags.explain_mode]] *)
let explain_mode = ref false
(*e: constant [[Flags.explain_mode]] *)

(* pad: I added this one *)
(*s: constant [[Flags.strict_mode]] *)
let strict_mode = ref false
(*e: constant [[Flags.strict_mode]] *)

(*s: constant [[Flags.dump_tokens]] *)
let dump_tokens = ref false
(*e: constant [[Flags.dump_tokens]] *)
(*s: constant [[Flags.dump_ast]] *)
let dump_ast = ref false
(*e: constant [[Flags.dump_ast]] *)
(*s: constant [[Flags.dump_env]] *)
let dump_env = ref false
(*e: constant [[Flags.dump_env]] *)
(*s: constant [[Flags.dump_graph]] *)
let dump_graph = ref false
(*e: constant [[Flags.dump_graph]] *)
(*s: constant [[Flags.dump_jobs]] *)
let dump_jobs = ref false
(*e: constant [[Flags.dump_jobs]] *)

(*s: constant [[Flags.debugger]] *)
let debugger = ref false
(*e: constant [[Flags.debugger]] *)
(*e: mk/Flags.ml *)
