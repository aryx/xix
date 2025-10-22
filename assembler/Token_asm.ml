type t =
  (* pseudo *)
  | TTEXT | TGLOBL
  | TDATA | TWORD
  (* virtual *)
  | TRET | TNOP
  (* registers *)
  | TR | TF
  | TPC | TSB | TFP | TSP
  | TRx of Ast_asm.register
  | TFx of Ast_asm.fregister
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
  (* for cpp; see also Parse_cpp.token_category *)
  | TSharp
  | EOF
