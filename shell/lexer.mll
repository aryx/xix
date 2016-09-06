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

let skipnl lexbuf = 
  print_string "TODO: skipnl"
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
  | '\n'      { TNewline }
  | '\\''\n'  {
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
  | "&&" { skipnl lexbuf; TAndAnd }
  | "||" { skipnl lexbuf; TOrOr }

  | "&"  { TAnd }
  | "|"  { TPipe }
  | ">"  { TRedir Ast.RWrite }
  | "<"  { TRedir Ast.RRead }
  | ">>" { TRedir Ast.RAppend }
  (* less: advanced pipe and redirection *)

  | ';'
  | '('  | ')'
  | '{'  | '}'

  | "~" { TTwiddle } 
  | "!" { TBang }
  | "@" { TSubshell }

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

(*****************************************************************************)
(* Quote rule *)
(*****************************************************************************)
and quote = parse
  | "'" { "" } 
  | eof { raise Todo }

