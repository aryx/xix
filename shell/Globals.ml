(*s: Globals.ml *)

(* This global is used by the lexer and parser to consider certain
 * newlines as regular spaces. Because rc is an interactive
 * interpreter, newline has a special meaning: it terminates a command.
 * However, sometimes we just want to add a newline because the command
 * is too long. Escaping the newline is one way to do it. 
 * But when we start to parse 'cmd1 &&' we know that we are expecting
 * more stuff. So after the && and other binary operators  we consider 
 * the newline not as a command terminator, but as a space.
 * Also we indent the prompt to let the user know he can still enter
 * more characters.
 *)
(*s: constant [[Globals.skipnl]] *)
let skipnl = ref false
(*e: constant [[Globals.skipnl]] *)

(* to mimic Plan 9 errstr() *)
(*s: constant [[Globals.errstr]] *)
let errstr = ref ""
(*e: constant [[Globals.errstr]] *)


(* Set to true at the beginning of an if and back to false
 * if rc executes the then branch. Kept to true otherwise
 * so next IfNot will run.
 *)
(*s: constant [[Globals.ifnot]] *)
let ifnot = ref false
(*e: constant [[Globals.ifnot]] *)

(* this is set after the first . of rcmain *)
(*s: constant [[Globals.eflagok]] *)
let eflagok = ref false
(*e: constant [[Globals.eflagok]] *)
(*e: Globals.ml *)
