
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
let skipnl = ref false

(* to mimic Plan 9 errstr() *)
let errstr = ref ""
