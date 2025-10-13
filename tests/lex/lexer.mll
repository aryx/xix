{
}

let space  = [' ''\t']

rule token = parse
  | "foo" { after_foo 0 lexbuf }

and after_foo myarg = parse
  | "bar" { after_foo (myarg + 1) lexbuf }
  | "foobar" { 2 }
  | "foox" { 3 }
