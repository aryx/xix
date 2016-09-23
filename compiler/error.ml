open Common

(* todo: delete outfile *)
let errorexit s =
  pr2 s;
  exit (-1)

(* let yyerror? use Globals.line? and Hist? *)
