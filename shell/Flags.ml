(*s: shell/Flags.ml *)

(*s: constant [[Flags.interactive]] *)
(* -i (on by default when detects that stdin is /dev/cons *)
let interactive = ref false
(*e: constant [[Flags.interactive]] *)
(*s: constant [[Flags.login]] *)
(* -l (on by default if argv0 starts with a -) *)
let login = ref false
(*e: constant [[Flags.login]] *)

(*s: constant [[Flags.eflag]] *)
(* -e, for strict error checking. Abort the script when an error happens.*)
let eflag = ref false
(*e: constant [[Flags.eflag]] *)
(*s: constant [[Flags.rflag]] *)
(* -r, similar to dump_opcodes, but at each step *)
let rflag = ref false
(*e: constant [[Flags.rflag]] *)
(*s: constant [[Flags.sflag]] *)
(* -s, to print status when error in command just ran *)
let sflag = ref false
(*e: constant [[Flags.sflag]] *)
(*s: constant [[Flags.xflag]] *)
(* -x, to print simple commands before executing them *)
let xflag = ref false
(*e: constant [[Flags.xflag]] *)

(* less: let cflag = ref "" *)

(*s: global [[Flags.hflags]] *)
let hflags: (char, bool) Hashtbl.t = Hashtbl.create 10
(*e: global [[Flags.hflags]] *)

(*s: constant [[Flags.rcmain]] *)
(* can be changed with -m *)
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
(*e: shell/Flags.ml *)
