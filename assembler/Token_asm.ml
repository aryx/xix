type t =
  (* pseudo *)
  | TTEXT | TGLOBL
  | TDATA | TWORD
  (* registers *)
  | TR
  | TPC | TSB | TFP | TSP
  | TRx of Ast_asm.register
  (* immediate *)
  | TINT of int
  | TFLOAT of float
  | TSTRING of string
  (* names *)
  | TIDENT of string
  (* punctuation *)
  | TSEMICOLON of int (* global line number *)
  | TCOLON | TDOT | TCOMMA | TDOLLAR
  | TOPAR | TCPAR
  (* operators *)
  | TPLUS | TMINUS
  | TMUL | TSLASH | TMOD
  (* for cpp *)
  | TSharp
  | EOF
