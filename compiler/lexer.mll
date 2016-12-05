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

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s =
  raise (L.Error (spf "Lexical error: %s" s, !L.line))

let loc () = !L.line


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

  (* compatibility with plan 9 C code? *)
  | 'f' -> pr2 "unknown \\f"; 0x00
  (* could be removed, special 5c escape char *)
  | 'a' -> 0x07 | 'v' -> 0x0b 

  | '\\' | '\'' | '"' -> Char.code c 
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
  | "+" { TPlus (loc()) } | "-" { TMinus (loc()) }
  | "*" { TMul  (loc()) } | "/" { TDiv   (loc()) } | "%" { TMod (loc()) }

  | "="  { TEq   (loc()) }
  | "==" { TEqEq (loc()) }  | "!=" { TBangEq (loc()) }
  | "&"  { TAnd  (loc()) }  | "|"  { TOr     (loc()) } | "^" { TXor (loc()) }
  | "~"  { TTilde  (loc()) }
  | "&&" { TAndAnd (loc()) } | "||" { TOrOr (loc()) }
  | "!"  { TBang   (loc()) }

  | "++" { TPlusPlus (loc()) } | "--" { TMinusMinus (loc()) }

  | "<"  { TInf   (loc()) } | ">"  { TSup   (loc()) }
  | "<=" { TInfEq (loc()) } | ">=" { TSupEq (loc()) }
  | "<<" { TInfInf (* TLsh *) (loc()) } | ">>" { TSupSup (* TRsh *) (loc()) }


  | "+=" { TOpEq (loc(), A.Plus) } | "-=" { TOpEq (loc(), A.Minus) }
  | "*=" { TOpEq (loc(), A.Mul) }  | "/=" { TOpEq (loc(), A.Div) }
  | "%=" { TOpEq (loc(), A.Mod) }
  | ">>="{ TOpEq (loc(), A.ShiftRight) } | "<<=" { TOpEq (loc(), A.ShiftLeft) }
  | "&=" { TOpEq (loc(), A.And) }        | "|="  { TOpEq (loc(), A.Or) }
  | "^=" { TOpEq (loc(), A.Xor) }

  | "(" { TOPar   (loc()) } | ")" { TCPar   (loc()) }
  | "{" { TOBrace (loc()) } | "}" { TCBrace (loc()) }
  | "[" { TOBra   (loc()) } | "]" { TCBra   (loc()) }
             
  | ","  { TComma (loc()) } | ";"  { TSemicolon (loc()) }
  | "->" { TArrow (loc()) }
  | "."  { TDot   (loc()) }
  | "?"  { TQuestion (loc()) }
  | ":"  { TColon    (loc()) }

  (* ----------------------------------------------------------------------- *)
  (* Numbers *)
  (* ----------------------------------------------------------------------- *)
  (* dup: lexer_asm5.mll *)
  | "0"  (oct+ as s) (['U''u']? as unsigned) (['L''l']* as long)
      { TIConst(loc(), "0o"^s, sign_of_suffix unsigned,intsize_of_suffix long)}
  | "0x" (hex+ as s)  (['U''u']? as unsigned) (['L''l']* as long)
      { TIConst(loc(), "0x"^s, sign_of_suffix unsigned, intsize_of_suffix long)}
  | "0x" { error "malformed hex constant" }

  | ['0'-'9'] digit* (['U''u']? as unsigned) (['L''l']* as long)
      { TIConst (loc(), Lexing.lexeme lexbuf, 
                sign_of_suffix unsigned, intsize_of_suffix long)
      }


  (* stricter: I impose some digit+ after '.' and after 'e' *)
  | ((digit+ | digit* '.' digit+) (['e''E'] ('+' | '-')? digit+)?) as s 
      (['F''f']* as float)
     { TFConst (loc(), s, floatsize_of_suffix float) }

  | (digit+ | digit* '.' digit+) ['e''E'] ('+' | '-')?
     { error "malformed fp constant exponent" }

  (* ----------------------------------------------------------------------- *)
  (* Strings and chars *)
  (* ----------------------------------------------------------------------- *)
  (* converting characters in integers *)
  | "'" { TIConst (loc(), spf "%d" (char lexbuf), Type.Signed, Storage.Char) }

  | '"' { TString (loc(), string lexbuf, Storage.String) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and identifiers *)
  (* ----------------------------------------------------------------------- *)
  | (letter | '_') (letter | digit | '_')* {
      let s = Lexing.lexeme lexbuf in
      match s with
      | "auto" -> Tauto (loc()) | "static" -> Tstatic (loc()) 
      | "extern" -> Textern (loc())
      | "register" -> Tregister (loc())

      | "const" -> Tconst (loc()) | "volatile" -> Tvolatile (loc())
      | "inline" -> Tinline (loc()) | "restrict" -> Trestrict (loc())

      | "void" -> Tvoid (loc())
      | "char" -> Tchar (loc()) | "short" -> Tshort (loc()) 
      | "int"  -> Tint  (loc()) | "long"  -> Tlong  (loc())
      | "float"  -> Tfloat  (loc()) | "double"   -> Tdouble   (loc())
      | "signed" -> Tsigned (loc()) | "unsigned" -> Tunsigned (loc())

      | "struct" -> Tstruct (loc()) | "union" -> Tunion (loc()) 
      | "enum" -> Tenum (loc())
      | "typedef" -> Ttypedef (loc())

      | "if" -> Tif (loc()) | "else" -> Telse (loc())
      | "while" -> Twhile (loc()) | "do" -> Tdo (loc()) | "for" -> Tfor (loc())
      | "break" -> Tbreak (loc()) | "continue" -> Tcontinue (loc())
      | "switch" -> Tswitch (loc()) 
      | "case" -> Tcase (loc()) | "default" -> Tdefault (loc())
      | "return" -> Treturn (loc()) | "goto" -> Tgoto (loc())

      | "sizeof" -> Tsizeof (loc())

      (* less: USED/SET here? or manage via symbol table *)

      | _ ->
        if Hashtbl.mem Globals.hids s
        then 
          (match Hashtbl.find Globals.hids s with
          | A.IdIdent | A.IdEnumConstant -> TName (loc(), s)
          | A.IdTypedef -> TTypeName (loc(), s)
          )
        else TName (loc(), s)
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
  | "\\" (['a'-'z' '\\' '"'] as c) 
      { let i = code_of_escape_char c in string_of_ascii i ^ string lexbuf  }
  (* strings can contain newline! but they must be escaped before *)
  | '\\' '\n' { "\n" ^ string lexbuf }
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
  | "\\" (['a'-'z' '\\' '\''] as c) "'"       { code_of_escape_char c }
  | '\\' '\n' { char lexbuf }
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
