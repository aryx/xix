(*s: Lexer.mll *)
{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Parser

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to rc:
 *  - no unicode support
*)
(*s: exception [[Lexer.Lexical_error]] *)
exception Lexical_error of string
(*e: exception [[Lexer.Lexical_error]] *)
(*s: function [[Lexer.error]] *)
let error s =
  raise (Lexical_error s)
(*e: function [[Lexer.error]] *)
(*s: function [[Lexer.incr_lineno]] *)
(* we could do that in pprompt() too *)
let incr_lineno () =
  let t = Runtime.cur () in
  incr t.Runtime.line
(*e: function [[Lexer.incr_lineno]] *)
}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
(*s: lexer regexp aliases *)
(* less: not used yet, special regexp used with lastdol *)
let idchr = ['a'-'z''A'-'Z''0'-'9''_''*']
(*x: lexer regexp aliases *)
(* original: !strchr("\n \t#;&|^$=`'{}()<>", c) && c!=EOF;
 * note that wordchr allows '~', '!', '@', '"', ','
 *)
let wordchr = [^'\n' ' ' '\t' '#'
                ';' '&''|''^' '$' '=' 
                '`' '\'' 
               '{''}' '('')' '<''>'
                ]
(*e: lexer regexp aliases *)

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)
(*s: rule [[Lexer.token]] *)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* Spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token()]] space cases *)
  | [' ''\t']+ { token lexbuf }
  (*x: [[Lexer.token()]] space cases *)
  | '\n'       { incr_lineno(); Prompt.doprompt := true; TNewline }
  (*x: [[Lexer.token()]] space cases *)
  | '\\''\n'  {
      incr_lineno ();
      Prompt.pprompt ();
      token lexbuf
  }    
  (*e: [[Lexer.token()]] space cases *)
  (*s: [[Lexer.token()]] comment cases *)
  | '#' [^'\n']* { token lexbuf }
  (*e: [[Lexer.token()]] comment cases *)

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token()]] symbol cases *)
  | ';'  { TSemicolon }
  (*x: [[Lexer.token()]] symbol cases *)
  | "&"  { TAnd }
  (*x: [[Lexer.token()]] symbol cases *)
  | ">"  { TRedir Ast.RWrite }
  | "<"  { TRedir Ast.RRead }
  | ">>" { TRedir Ast.RAppend }
  (*x: [[Lexer.token()]] symbol cases *)
  | '('  { TOPar }   | ')' { TCPar }
  | '{'  { TOBrace } | '}' { TCBrace }
  (*x: [[Lexer.token()]] symbol cases *)
  | '='  { TEq }
  (*x: [[Lexer.token()]] symbol cases *)
  | "|"  { Globals.skipnl := true; TPipe }
  (*x: [[Lexer.token()]] symbol cases *)
  | "&&" { Globals.skipnl := true; TAndAnd }
  | "||" { Globals.skipnl := true; TOrOr }
  (*x: [[Lexer.token()]] symbol cases *)
  | "!" { TBang }
  (*x: [[Lexer.token()]] symbol cases *)
  | "~" { TTwiddle } 
  (*x: [[Lexer.token()]] symbol cases *)
  | "@" { TSubshell }
  (*x: [[Lexer.token()]] symbol cases *)
  | ">[" (['0'-'9']+ (*as fd0*)) "=" (['0'-'9']+ (*as fd1*)) "]" 
         {  let fd0 = failwith "TODO: fd0" in
     let fd1 = failwith "TODO: fd1" in
     TDup (Ast.RWrite, int_of_string fd0, int_of_string fd1)
  }
  | ">>[" (['0'-'9']+ (*as fd0*)) "=" (['0'-'9']+ (*as fd1*)) "]" 
         {
             let fd0 = failwith "TODO: fd0" in
      let fd1 = failwith "TODO: fd1" in
      TDup (Ast.RAppend, int_of_string fd0, int_of_string fd1)
  }
  (* less: advanced pipe and redirection *)
  (*x: [[Lexer.token()]] symbol cases *)
  | "`" { TBackquote } 
  (*x: [[Lexer.token()]] symbol cases *)
  | "^" { TCaret }
  (*e: [[Lexer.token()]] symbol cases *)

  (* ----------------------------------------------------------------------- *)
  (* Variables *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token()]] variable cases *)
  | "$"   { TDollar }
  (*x: [[Lexer.token()]] variable cases *)
  | "$\"" { TStringify }
  (*x: [[Lexer.token()]] variable cases *)
  | "$#"  { TCount }
  (*e: [[Lexer.token()]] variable cases *)

  (* ----------------------------------------------------------------------- *)
  (* Quoted word *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token()]] quoted word cases *)
  | "'" { let s = quote lexbuf in TWord (s, true) }
  (*e: [[Lexer.token()]] quoted word cases *)

  (* ----------------------------------------------------------------------- *)
  (* Keywords and unquoted words *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token()]] keywords and unquoted words cases *)
  | wordchr+ { 

    let s = Lexing.lexeme lexbuf in
    (match s with
    | "if"    -> TIf
    | "while" -> TWhile
    | "for"   -> TFor
    | "in"    -> TIn
    | "not"   -> TNot
    | "switch" -> TSwitch
    | "fn"     -> TFn

    | _ -> TWord (s, false)
    )
  }
  (*e: [[Lexer.token()]] keywords and unquoted words cases *)

  (* ----------------------------------------------------------------------- *)
  | eof { EOF }
  | _ (*as c*)   { error (spf "unrecognized character: '%s'" (Lexing.lexeme lexbuf)) }
(*e: rule [[Lexer.token]] *)

(*****************************************************************************)
(* Quote rule *)
(*****************************************************************************)
(*s: rule [[Lexer.quote]] *)
and quote = parse
  | "'" { "" } 
  | "''" { "'" ^ quote lexbuf } 
  | "\n" { 
      incr_lineno ();
      Prompt.pprompt ();
      "\n" ^ quote lexbuf
    }
  | [^'\'' '\n']+ { let s = Lexing.lexeme lexbuf in s ^ quote lexbuf }
  (* stricter: generate error *)
  | eof { error "unterminated quote" }
(*e: rule [[Lexer.quote]] *)
(*e: Lexer.mll *)
