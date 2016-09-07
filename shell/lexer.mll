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

exception Lexical_error of string
let error s =
  raise (Lexical_error s)

let skipnl lexbuf = 
  print_string "TODO: skipnl"

let incr_lineno () =
  let t = Runtime.cur () in
  incr t.Runtime.line
  
}


(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
(* original: !strchr("\n \t#;&|^$=`'{}()<>", c) && c!=EOF;
 * note that wordchr allows '~', '!', '@', '"', ','
 *)
let wordchr = [^'\n' ' ' '\t' '#'
                ';' '&''|''^' '$' '=' 
                '`' '\'' 
               '{''}' '('')' '<''>'
                ]

let idchr = ['a'-'z''A'-'Z''0'-'9''_''*']

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* Spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | [' ''\t']+ { token lexbuf }
  | '\n'       { incr_lineno(); TNewline }
  | '\\''\n'  {
      incr_lineno ();
      (* note: cannot call 'token lexbuf' otherwill will not

       * get the prompt if what is after the newline is a set of spaces.
       * Need the prompt ASAP. So return a Fake token that is
       * interpreted in a special way in the caller of token.
       *)
      raise Todo
  }    
  | '#' [^'\n']* { token lexbuf }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  | "&&" { Globals.skipnl := true; TAndAnd }
  | "||" { Globals.skipnl := true; TOrOr }

  | "&"  { TAnd }
  | "|"  { Globals.skipnl := true; TPipe }
  | ">"  { TRedir Ast.RWrite }
  | "<"  { TRedir Ast.RRead }
  | ">>" { TRedir Ast.RAppend }
  | ">[" (['0'-'9']+ as fd0) "=" (['0'-'9']+ as fd1) "]" 
         { TDup (Ast.RWrite, int_of_string fd0, int_of_string fd1) }
  | ">>[" (['0'-'9']+ as fd0) "=" (['0'-'9']+ as fd1) "]" 
         { TDup (Ast.RAppend, int_of_string fd0, int_of_string fd1) }
  (* less: advanced pipe and redirection *)

  | ';'  { TSemicolon }
  | '='  { TEq }
  | '('  { TOPar }   | ')' { TCPar }
  | '{'  { TOBrace } | '}' { TCBrace }

  | "`" { TBackquote } 

  | "~" { TTwiddle } 
  | "!" { TBang }
  | "@" { TSubshell }

  | "^" { TCaret }

  (* ----------------------------------------------------------------------- *)
  (* Variables *)
  (* ----------------------------------------------------------------------- *)

  | "$"   { TDollar }
  | "$#"  { TCount }
  | "$\"" { TStringify }

  (* ----------------------------------------------------------------------- *)
  (* Quoted word *)
  (* ----------------------------------------------------------------------- *)
  | "'" { let s = quote lexbuf in TWord (s, true) }

  (* ----------------------------------------------------------------------- *)
  (* Ketwords and unquoted words *)
  (* ----------------------------------------------------------------------- *)

  | wordchr+ { 

    let s = Lexing.lexeme lexbuf in
    (match s with
    | "for"   -> TFor
    | "in"    -> TIn
    | "while" -> TWhile
    | "if"    -> TIf
    | "not"   -> TNot
    | "switch" -> TSwitch
    | "fn"     -> TFn

    | _ -> TWord (s, false)
    )
  }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF }
  | _ as c   { error (spf "unrecognized character: '%c'" c) }

(*****************************************************************************)
(* Quote rule *)
(*****************************************************************************)
and quote = parse
  | "''" { "'" ^ quote lexbuf } 
  | "'" { "" } 
  | "\n" { 
      incr_lineno ();
      "\n" ^ quote lexbuf
    }
  | [^'\'' '\n']+ { let s = Lexing.lexeme lexbuf in s ^ quote lexbuf }
  (* stricter: generate error *)
  | eof { error "unterminated quote" }

