{
(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Token_asm
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Common parts of the different Plan 9 assembly lexers.
 * 
 * Limitations compared to 5a/va/...:
 *  - no unicode support
 *  - no uU lL suffix
 *    (but was skipped by 5a anyway)
 *  - no '=' used for constant definition
 *    (but can use cpp #define for that)
 *)

let error s =
  raise (L.Error (spf "Lexical error: %s" s, !L.line))

let code_of_escape_char c =
  match c with
  | 'n' -> Char.code '\n' | 'r' -> Char.code '\r' 
  | 't' -> Char.code '\t' | 'b' -> Char.code '\b' 

  | 'f' -> error "unknown \\f"
  (* could be removed, special 5a escape char *)
  | 'a' -> 0x07 | 'v' -> 0x0b 
  (* useful to generate C-compatible strings with special end marker *)
  | 'z' -> 0

  (* stricter: we disallow \ with unknown character *)
  | _ -> error "unknown escape sequence"

let string_of_ascii i =
  String.make 1 (Char.chr i)

(* needed only because of ocamllex limitations in ocaml-light
 * which does not support the 'as' feature.
 *)
let char_ lexbuf =
  let s = Lexing.lexeme lexbuf in
  String.get s 0
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
  | "R" (digit+ (*as s*)) 
      { let s = Lexing.lexeme lexbuf |> String_.drop_prefix 1 in
        let i = int_of_string s in 
        (* the range check is arch-specific and must be done in Parse_asmX.ml *)
        TRx (Ast_asm.R i)
      }
  | "F" (digit+ (*as s*)) 
      { let s = Lexing.lexeme lexbuf |> String_.drop_prefix 1 in
        let i = int_of_string s in 
        (* the range check is arch-specific and must be done in Parse_asmX.ml *)
        TFx (Ast_asm.F i)
      }

  (* looser: actually for '.' 5a imposes to have an isalpha() after *)    
  | (letter | '_' | '@' | '.') (letter | digit | '_' | '$' )* {
      let s = Lexing.lexeme lexbuf in
      (* fast enough? I hope OCaml generate good code for strings matching
       * alt: use Hashtbl.t
       *)
      match s with
      (* pseudo instructions *)
      | "TEXT" -> TTEXT | "GLOBL" -> TGLOBL
      | "WORD" -> TWORD | "DATA" -> TDATA

      (* virtual instructions *)
      | "RET" -> TRET
      | "NOP" -> TNOP
 
      (* registers (see also the special rule above for R digit+) *)
      | "R" -> TR
      | "F" -> TF

      (* pseudo registers *)
      | "PC" -> TPC | "SB" -> TSB | "SP" -> TSP | "FP" -> TFP

      (* less: could impose is_lowercase? *)
      | _ -> TIDENT s
    }

  (* ----------------------------------------------------------------------- *)
  (* Numbers *)
  (* ----------------------------------------------------------------------- *)
  | "0"  (oct+ (*as s*)) 
      { let s = Lexing.lexeme lexbuf |> String_.drop_prefix 1 in
        TINT (int_of_string ("0o" ^ s))  }
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
  (* See ../macroprocessor/Lexer_cpp.mll (called from Parse_asm5.ml) *)
  | "#" { TSharp }

  (* ----------------------------------------------------------------------- *)
  (* less: maybe return a fake semicolon the first time? *)
  | eof { EOF }
  | _ (*as c*)   { let c = char_ lexbuf in
                   error (spf "unrecognized character: '%c'" c) }

(*****************************************************************************)
(* Rule char *)
(*****************************************************************************)
and char = parse
  | "''"                            { Char.code '\'' }
  | "\\" ((oct oct? oct?) (*as s*)) "'" 
      { let s = Lexing.lexeme lexbuf |>
           String_.drop_prefix 1 |> String_.drop_suffix 1 in
         int_of_string ("0o" ^ s) }
  | "\\" (['a'-'z'] (*as c*)) "'"       
     { let c = String.get (Lexing.lexeme lexbuf) 1 in
       code_of_escape_char c }
  | [^ '\\' '\'' '\n'] (*as c*)  "'"    
      { let c = String.get (Lexing.lexeme lexbuf) 0 in
        Char.code c }
  | '\n' { error "newline in character" }
  | eof  { error "end of file in character" }
  | _    { error "missing '" }

(*****************************************************************************)
(* Rule string *)
(*****************************************************************************)
and string = parse
  | '"' { "" }
  | "\\" ((oct oct oct) (*as s*))
      { let s = Lexing.lexeme lexbuf |> String_.drop_prefix 1 in
        let i = int_of_string ("0o" ^ s) in string_of_ascii i ^ string lexbuf }
  | "\\" (['a'-'z'] (*as c*)) 
      { let c = String.get (Lexing.lexeme lexbuf) 1 in
        let i = code_of_escape_char c in string_of_ascii i ^ string lexbuf  }
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
