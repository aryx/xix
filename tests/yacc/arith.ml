type token =
 | PLUS
 | MULT
 | TOPAR
 | TCPAR
 | ID of string

let lrtables = {
  Parsing.action = (function
   | S 1, TOPAR _ -> Parsing.Shift 7
   | S 1, ID _ -> Parsing.Shift 5
   | S 2, MULT _ -> Parsing.Reduce(3, NT "t", RA "t")
   | S 2, PLUS _ -> Parsing.Reduce(3, NT "t", RA "t")
   | S 2, TCPAR _ -> Parsing.Reduce(3, NT "t", RA "t")
   | S 3, MULT _ -> Parsing.Reduce(1, NT "t", RA "t")
   | S 3, PLUS _ -> Parsing.Reduce(1, NT "t", RA "t")
   | S 3, TCPAR _ -> Parsing.Reduce(1, NT "t", RA "t")
   | S 4, MULT _ -> Parsing.Reduce(3, NT "f", RA "f")
   | S 4, PLUS _ -> Parsing.Reduce(3, NT "f", RA "f")
   | S 4, TCPAR _ -> Parsing.Reduce(3, NT "f", RA "f")
   | S 5, MULT _ -> Parsing.Reduce(1, NT "f", RA "f")
   | S 5, PLUS _ -> Parsing.Reduce(1, NT "f", RA "f")
   | S 5, TCPAR _ -> Parsing.Reduce(1, NT "f", RA "f")
   | S 6, PLUS _ -> Parsing.Shift 9
   | S 7, TOPAR _ -> Parsing.Shift 7
   | S 7, ID _ -> Parsing.Shift 5
   | S 8, PLUS _ -> Parsing.Shift 9
   | S 8, TCPAR _ -> Parsing.Shift 4
   | S 9, TOPAR _ -> Parsing.Shift 7
   | S 9, ID _ -> Parsing.Shift 5
   | S 10, PLUS _ -> Parsing.Reduce(3, NT "e", RA "e")
   | S 10, TCPAR _ -> Parsing.Reduce(3, NT "e", RA "e")
   | S 10, MULT _ -> Parsing.Shift 1
   | S 11, PLUS _ -> Parsing.Reduce(1, NT "e", RA "e")
   | S 11, TCPAR _ -> Parsing.Reduce(1, NT "e", RA "e")
   | S 11, MULT _ -> Parsing.Shift 1
   | S 0, TOPAR _ -> Parsing.Shift 7
   | S 0, ID _ -> Parsing.Shift 5
    | _ -> raise Parsing.Parse_error
  );
  Parsing.goto = (function
  | S 1, NT "f" -> S 2
  | S 7, NT "e" -> S 8
  | S 7, NT "f" -> S 3
  | S 7, NT "t" -> S 11
  | S 9, NT "f" -> S 3
  | S 9, NT "t" -> S 10
  | S 0, NT "e" -> S 6
  | S 0, NT "f" -> S 3
  | S 0, NT "t" -> S 11
    | _ -> raise Parsing.Parse_error
  );
}
let e lexfun lexbuf =
  Parsing.yyparse_simple lrtables lexfun lexbuf
