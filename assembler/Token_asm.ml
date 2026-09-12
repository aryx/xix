(*s: Token_asm.ml *)
(*s: type [[Token_asm.t]] *)
type t =
  (* pseudo *)
  | TTEXT | TGLOBL
  | TWORD | TDATA 
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
  (* claude: case 38/39 (ARM MOVM's "[R4-R11,R14]" register-list
   * operand) -- shared in Token_asm.ml/Lexer_asm.mll like every other
   * punctuation token, even though only Parser_asm5.mly's grammar
   * actually uses it (i/v's Parse_asmX.ml just pass it through, same
   * as any other token their own grammar doesn't reference). *)
  | TLBRACKET | TRBRACKET
  (* operators *)
  | TPLUS | TMINUS
  | TMUL | TSLASH | TMOD
  (* for cpp; see also Parse_cpp.token_category *)
  | TSharp
  | EOF
(*e: type [[Token_asm.t]] *)
(*e: Token_asm.ml *)
