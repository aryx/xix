
(* -i (on by default when detects that stdin is /dev/cons *)
let interactive = ref false
(* -l (on by default if argv0 starts with a -) *)
let login = ref false

(* -e, for strict error checking. Abort the script when an error happens.*)
let eflag = ref false
(* -r, similar to dump_opcodes, but at each step *)
let rflag = ref false
(* -s, to print status when error in command just ran *)
let sflag = ref false
(* -x, to print simple commands before executing them *)
let xflag = ref false

(* less: let cflag = ref "" *)

let (hflags: (char, bool) Hashtbl.t) = Hashtbl.create 10

(* can be changed with -m *)
let rcmain = ref "/rc/lib/rcmain"

(* pad: I added this one *)
let strict_mode = ref false

let dump_tokens = ref false
let dump_ast = ref false
let dump_opcodes = ref false

let debugger = ref false
