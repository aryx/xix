{
(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm5
open Parser_asm5
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to 5a:
 *  - does not handle unicode
 *  - does not recognize the uU lL suffix 
 *    (but was skipped by 5a anyway)
 *)

let error s =
  raise (L.Error (spf "Lexical error: %s" s, !L.line))

(* stricter: we disallow \ with unknown character *)
let code_of_escape_char c =
  match c with
  | 'n' -> Char.code '\n' | 'r' -> Char.code '\r' 
  | 't' -> Char.code '\t' | 'b' -> Char.code '\b' 

  | 'f' -> error "unknown \\f"
  (* could be removed, special 5a escape char *)
  | 'a' -> 0x07 | 'v' -> 0x0b | 'z' -> 0

  | _ -> error "unknown escape sequence"

let string_of_ascii i =
  String.make 1 (Char.chr i)

}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let letter = ['a'-'z''A'-'Z']
let space = [' ''\t']
let digit = ['0'-'9']
let hex = (digit | ['A'-'F''a'-'f'])
let oct = ['0'-'7']

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* Spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | space+ { token lexbuf }

  | "//" [^'\n']* { token lexbuf }
  | "/*"          { comment lexbuf }

  (* newlines are converted in fake semicolons for the grammar *)
  | '\n' { let old = !L.line in incr L.line; TSEMICOLON old }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  | ';' { TSEMICOLON !L.line }

  | ':' { TCOLON }
  | ',' { TCOMMA }
  | '(' { TOPAR } | ')' { TCPAR }
  | '$' { TDOLLAR }

  | '+' { TPLUS } | '-' { TMINUS } 
  (* '/' is used for division and for DATA too *)
  | '*' { TMUL }  | '/' { TSLASH } | '%' { TMOD }

  (* has to be before the rule for identifiers *)
  | '.' { TDOT }

  (* ----------------------------------------------------------------------- *)
  (* Mnemonics and identifiers *)
  (* ----------------------------------------------------------------------- *)
  | "R" (digit+ as s) 
      { let i = int_of_string s in 
        if i <= 15 && i >=0
        then TRx (R i)
        else error ("register number not valid")
      }

  (* looser: actually for '.' 5a imposes to have an isalpha() after *)    
  | (letter | '_' | '@' | '.') (letter | digit | '_' | '$' )* {
      let s = Lexing.lexeme lexbuf in
      (* fast enough? I hope ocaml generate good code for strings matching *)
      match s with
      (* instructions *)
      | "AND" -> TARITH AND | "ORR" -> TARITH ORR | "EOR" -> TARITH EOR

      | "ADD" -> TARITH ADD | "SUB" -> TARITH SUB
      | "MUL" -> TARITH MUL | "DIV" -> TARITH DIV | "MOD" -> TARITH MOD
      | "SLL" -> TARITH SLL | "SRL" -> TARITH SRL | "SRA" -> TARITH SRA

      | "BIC" -> TARITH BIC
      | "ADC" -> TARITH ADC | "SBC" -> TARITH SBC
      | "RSB" -> TARITH RSB | "RSC" -> TARITH RSC

      | "MVN" -> TMVN

      | "MOVW" -> TMOV Word
      | "MOVB" -> TMOV (Byte     Signed) | "MOVBU" -> TMOV (Byte     Unsigned)
      | "MOVH" -> TMOV (HalfWord Signed) | "MOVHU" -> TMOV (HalfWord Unsigned)

      | "B" -> TB | "BL" -> TBL
      | "CMP" -> TCMP CMP 
      | "TST" -> TCMP TST | "TEQ" -> TCMP TEQ | "CMN" -> TCMP CMN
      | "RET" -> TRET
      
      | "BEQ" -> TBx EQ | "BNE" -> TBx NE
      | "BGT" -> TBx (GT Signed) | "BLT" -> TBx (LT Signed)
      | "BGE" -> TBx (GE Signed) | "BLE" -> TBx (LE Signed)
      | "BHI" -> TBx (GT Unsigned) | "BLO" -> TBx (LT Unsigned) 
      | "BHS" -> TBx (GE Unsigned) | "BLS" -> TBx (LE Unsigned)
      | "BMI" -> TBx MI | "BPL" -> TBx PL 
      | "BVS" -> TBx VS | "BVC" -> TBx VC

      | "SWI" -> TSWI
      | "RFE" -> TRFE

      (* pseudo instructions *)
      | "TEXT" -> TTEXT | "GLOBL" -> TGLOBL
      | "WORD" -> TWORD | "DATA" -> TDATA

      (* registers (see also the special rule above for R digit+) *)
      | "R" -> TR

      (* pseudo registers *)
      | "PC" -> TPC | "SB" -> TSB | "SP" -> TSP | "FP" -> TFP

      (* conditions *)
      | ".EQ" -> TCOND EQ | ".NE" -> TCOND NE
      | ".GT" -> TCOND (GT Signed)   | ".LT" -> TCOND (LT Signed) 
      | ".GE" -> TCOND (GE Signed)   | ".LE" -> TCOND (LE Signed)
      | ".HI" -> TCOND (GT Unsigned) | ".LO" -> TCOND (LT Unsigned)
      | ".HS" -> TCOND (GE Unsigned) | ".LS" -> TCOND (LE Unsigned)
      | ".MI" -> TCOND MI | ".PL" -> TCOND PL 
      | ".VS" -> TCOND VS | ".VC" -> TCOND VC

      (* less: special bits *)
      (* less: float, MUL, ... *)

      (* less: could impose is_lowercase? *)
      | _ -> TIDENT s
    }

  (* ----------------------------------------------------------------------- *)
  (* Numbers *)
  (* ----------------------------------------------------------------------- *)
  | "0"  (oct+ as s) { TINT (int_of_string ("0o" ^ s))  }
  | "0x" hex+        { TINT (int_of_string (Lexing.lexeme lexbuf)) }
  | digit+           { TINT (int_of_string (Lexing.lexeme lexbuf)) }

  (* stricter: I impose some digit+ after '.' and after 'e' *)
  | (digit+ | digit* '.' digit+) (['e''E'] ('+' | '-')? digit+)?
     { TFLOAT (float_of_string (Lexing.lexeme lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* Chars/Strings *)
  (* ----------------------------------------------------------------------- *)
  | "'" { TINT (char lexbuf) }
  | '"' 
      { let s = string lexbuf in
        (* less: why this limit though? *)
        if String.length s > 8
        then error ("string constant too long")
        else TSTRING s
      }

  (* ----------------------------------------------------------------------- *)
  (* CPP *)
  (* ----------------------------------------------------------------------- *)
  (* See ../macroprocessor/lexer_cpp.mll *)
  | "#" { TSharp }

  (* ----------------------------------------------------------------------- *)
  (* less: maybe return a fake semicolon the first time? *)
  | eof { EOF }
  | _ as c   { error (spf "unrecognized character: '%c'" c) }

(*****************************************************************************)
(* Rule char *)
(*****************************************************************************)
and char = parse
  | "''"                            { Char.code '\'' }
  | "\\" ((oct oct? oct?) as s) "'" { int_of_string ("0o" ^ s) }
  | "\\" (['a'-'z'] as c) "'"       { code_of_escape_char c }
  | [^ '\\' '\'' '\n'] as c  "'"    { Char.code c }
  | '\n' { error "newline in character" }
  | eof  { error "end of file in character" }
  | _    { error "missing '" }

(*****************************************************************************)
(* Rule string *)
(*****************************************************************************)
and string = parse
  | '"' { "" }
  | "\\" ((oct oct oct) as s)
      { let i = int_of_string ("0o" ^ s) in string_of_ascii i ^ string lexbuf }
  | "\\" (['a'-'z'] as c) 
      { let i = code_of_escape_char c in string_of_ascii i ^ string lexbuf  }
  | [^ '\\' '"' '\n']+   
      { let x = Lexing.lexeme lexbuf in x ^ string lexbuf }
  | '\n' { error "newline in string" }
  | eof  { error "end of file in string" }
  | _    { error "undefined character in string" }

(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)
and comment = parse
  | "*/"          { token lexbuf }
  | [^ '*' '\n']+ { comment lexbuf }
  | '*'           { comment lexbuf }
  | '\n'          { incr L.line; comment lexbuf }
  | eof           { error "end of file in comment" }
