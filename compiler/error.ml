open Common

exception Error of string * Location_cpp.loc

(* todo: delete outfile *)
let errorexit s =
  pr2 s;
  exit (-1)
