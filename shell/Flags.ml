(*s: Flags.ml *)

(* -i (on by default when detects that stdin is /dev/cons *)
(*s: constant [[Flags.interactive]] *)
let interactive = ref false
(*e: constant [[Flags.interactive]] *)
(* -l (on by default if argv0 starts with a -) *)
(*s: constant [[Flags.login]] *)
let login = ref false
(*e: constant [[Flags.login]] *)

(* -e, for strict error checking. Abort the script when an error happens.*)
(*s: constant [[Flags.eflag]] *)
let eflag = ref false
(*e: constant [[Flags.eflag]] *)
(* -r, similar to dump_opcodes, but at each step *)
(*s: constant [[Flags.rflag]] *)
let rflag = ref false
(*e: constant [[Flags.rflag]] *)
(* -s, to print status when error in command just ran *)
(*s: constant [[Flags.sflag]] *)
let sflag = ref false
(*e: constant [[Flags.sflag]] *)
(* -x, to print simple commands before executing them *)
(*s: constant [[Flags.xflag]] *)
let xflag = ref false
(*e: constant [[Flags.xflag]] *)

(* less: let cflag = ref "" *)

let (hflags: (char, bool) Hashtbl.t) = Hashtbl.create 10

(* can be changed with -m *)
(*s: constant [[Flags.rcmain]] *)
let rcmain = ref "/rc/lib/rcmain"
(*e: constant [[Flags.rcmain]] *)

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
(*s: constant [[Flags.dump_opcodes]] *)
let dump_opcodes = ref false
(*e: constant [[Flags.dump_opcodes]] *)

(*s: constant [[Flags.debugger]] *)
let debugger = ref false
(*e: constant [[Flags.debugger]] *)
(*e: Flags.ml *)
