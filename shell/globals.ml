
(* This is a global used by the lexer and parser to consider certain
 * newlines as regular space. Because rc is an interactive
 * interpreter, newline has a special meaning: it terminates a command.
 * However, sometimes we just want to add a newline because the command
 * is too long. Escaping the newline is one way to do it. 
 * But when we start to parse 'cmd1 &&' we know that we are expecting
 * more stuff. So after the && and other binary operators  we consider 
 * the newline not as* a command terminator, but as a space.
 *)
let skipnl = ref false

(* to mimic plan9 errstr() *)
let errstr = ref ""
