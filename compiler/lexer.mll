{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Parser
module A = Ast
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to 5c:
 *  - no L"" and L'' unicode strings or characters
 *  - no \x hexadecimal escape sequence in strings or characters
 *  - no unicode identifier
 *)

let error s =
  raise (L.Error (spf "Lexical error: %s" s, !L.line))


let sign_of_suffix s =
  match String.lowercase s with
  | "" -> Type.Signed
  | "u" -> Type.Unsigned
  | s -> error (spf "Impossible: wrong sign suffix: %s" s)

let intsize_of_suffix s =
  match String.lowercase s with
  | "" -> Storage.Int
  | "l" -> Storage.Long
  | "ll" -> Storage.VLong
  | s -> error (spf "Impossible: wrong int size suffix: %s" s)

let floatsize_of_suffix s =
  match String.lowercase s with
  | "" -> Storage.Double
  | "f" -> Storage.Float
  | s -> error (spf "Impossible: wrong float size suffix: %s" s)

(* dup: lexer_asm5.mll *)
(* stricter: we disallow \ with unknown character *)
let code_of_escape_char c =
  match c with
  | 'n' -> Char.code '\n' | 'r' -> Char.code '\r' 
  | 't' -> Char.code '\t' | 'b' -> Char.code '\b' 

  | 'f' -> error "unknown \\f"
  (* could be removed, special 5c escape char *)
  | 'a' -> 0x07 | 'v' -> 0x0b 
  | _ -> error "unknown escape sequence"

(* dup: lexer_asm5.mll *)
let string_of_ascii i =
  String.make 1 (Char.chr i)

}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let space = [' ''\t']
let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let oct = ['0'-'7']
let hex = (digit | ['A'-'F''a'-'f'])

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* Spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | [' ''\t']+    { token lexbuf }
  | "//" [^'\n']* { token lexbuf }
  | "/*"          { comment lexbuf }

  | '\n'          { incr Location_cpp.line; token lexbuf }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  | "+" { TPlus } | "-" { TMinus }
  | "*" { TMul }  | "/" { TDiv } | "%" { TMod }

  | "="  { TEq } 
  | "==" { TEqEq } | "!=" { TBangEq }
  | "&"  { TAnd }  | "|"   { TOr } | "^" { TXor }
  | "~"  { TTilde }
  | "&&" { TAndAnd } | "||" { TOrOr }
  | "!"  { TBang } 

  | "++" { TPlusPlus } | "--" { TMinusMinus }  

  | "<"  { TInf }   | ">" { TSup }
  | "<=" { TInfEq } | ">=" { TSupEq }
  | "<<" { TInfInf (* TLsh *) } | ">>" { TSupSup (* TRsh *) }


  | "+=" { TOpEq A.Plus }       | "-=" { TOpEq A.Minus } 
  | "*=" { TOpEq A.Mul }        | "/=" { TOpEq A.Div } | "%=" { TOpEq A.Mod }
  | ">>="{ TOpEq A.ShiftRight } | "<<=" { TOpEq A.ShiftLeft }
  | "&=" { TOpEq A.And }        | "|=" { TOpEq A.Or }  | "^=" { TOpEq A.Xor }

  | "(" { TOPar }   | ")" { TCPar }
  | "{" { TOBrace } | "}" { TCBrace }
  | "[" { TOBra }   | "]" { TCBra }
             
  | "," { TComma } | ";"  { TSemicolon }
  | "->" { TArrow }
  | "."  { TDot }
  | "?"  { TQuestion }
  | ":"  { TColon }

  (* ----------------------------------------------------------------------- *)
  (* Numbers *)
  (* ----------------------------------------------------------------------- *)
  (* dup: lexer_asm5.mll *)
  | "0"  (oct+ as s) (['U''u']? as unsigned) (['L''l']* as long)
      { TConst ("0o" ^ s, sign_of_suffix unsigned, intsize_of_suffix long) }
  | "0x" (hex+ as s)  (['U''u']? as unsigned) (['L''l']* as long)
      { TConst ("0x" ^ s, sign_of_suffix unsigned, intsize_of_suffix long) }
  | "0x" { error "malformed hex constant" }

  | ['1'-'9'] digit* (['U''u']? as unsigned) (['L''l']* as long)
      { TConst (Lexing.lexeme lexbuf, 
                sign_of_suffix unsigned, intsize_of_suffix long)
      }


  (* stricter: I impose some digit+ after '.' and after 'e' *)
  | ((digit+ | digit* '.' digit+) (['e''E'] ('+' | '-')? digit+)?) as s 
      (['F''f']* as float)
     { TFConst (s, floatsize_of_suffix float) }

  | (digit+ | digit* '.' digit+) ['e''E'] ('+' | '-')?
     { error "malformed fp constant exponent" }

  (* ----------------------------------------------------------------------- *)
  (* Strings and chars *)
  (* ----------------------------------------------------------------------- *)
  | "'" { TConst (spf "%d" (char lexbuf), Type.Signed, Storage.Char) }

  | '"' { TString (string lexbuf, Storage.String) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and identifiers *)
  (* ----------------------------------------------------------------------- *)
  | (letter | '_') (letter | digit | '_')* {
      let s = Lexing.lexeme lexbuf in
      match s with
      | "auto" -> Tauto | "static" -> Tstatic | "extern" -> Textern
      | "register" -> Tregister

      | "const" -> Tconst | "volatile" -> Tvolatile
      | "inline" -> Tinline | "restrict" -> Trestrict

      | "void" -> Tvoid
      | "char" -> Tchar | "short" -> Tshort | "int" -> Tint | "long" -> Tlong
      | "float" -> Tfloat | "double" -> Tdouble
      | "signed" -> Tsigned | "unsigned" -> Tunsigned

      | "struct" -> Tstruct | "union" -> Tunion | "enum" -> Tenum
      | "typedef" -> Ttypedef

      | "if" -> Tif | "else" -> Telse 
      | "while" -> Twhile | "do" -> Tdo | "for" -> Tfor 
      | "break" -> Tbreak | "continue" -> Tcontinue
      | "switch" -> Tswitch | "case" -> Tcase | "default" -> Tdefault
      | "return" -> Treturn | "goto" -> Tgoto

      | "sizeof" -> Tsizeof

      | _ ->
        if Hashtbl.mem Globals.hids s
        then 
          (match Hashtbl.find Globals.hids s with
          | A.IdIdent | A.IdEnumConstant -> TName s
          | A.IdTypedef -> TTypeName s
          )
        else TName s
  }

  (* ----------------------------------------------------------------------- *)
  (* CPP *)
  (* ----------------------------------------------------------------------- *)
  (* See ../macroprocessor/lexer_cpp.mll *)
  | "#" { TSharp }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF }
  | _ as c   { error (spf "unrecognized character: '%c'" c) }

(*****************************************************************************)
(* String rule *)
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
(* Character rule *)
(*****************************************************************************)
and char = parse
  | "''"                            { Char.code '\'' }
  (* less: 5c allows up to 8 octal number when in L'' mode *)
  | "\\" ((oct oct? oct?) as s) "'" { int_of_string ("0o" ^ s) }
  | "\\" (['a'-'z'] as c) "'"       { code_of_escape_char c }
  | [^ '\\' '\'' '\n'] as c  "'"    { Char.code c }
  | '\n' { error "newline in character" }
  | eof  { error "end of file in character" }
  | _    { error "missing '" }

(*****************************************************************************)
(* Comment rule *)
(*****************************************************************************)

(* dup: lexer_asm5.mll *)
and comment = parse
  | "*/"          { token lexbuf }
  | [^ '*' '\n']+ { comment lexbuf }
  | '*'           { comment lexbuf }
  | '\n'          { incr Location_cpp.line; comment lexbuf }
  | eof           { error "eof in comment" }
