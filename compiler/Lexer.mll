(*s: Lexer.mll *)
{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Regexp.Operators

open Parser
module A = Ast
module L = Location_cpp
module T = Type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to 5c:
 *  - no L"" and L'' unicode strings or unicode characters
 *  - no \x hexadecimal escape sequence in strings or characters
 *  - no unicode identifier
 *  - no typestr
 *    (seems dead extension anyway)
 *  - no signof
 * 
 * todo: handle big numbers here? or let later phases do that?
 *  check for overflow, truncation, sign-extended character constant, etc
 *  see yyerror and warn in Compiler.nw
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: Lexer helpers *)
(*s: function [[Lexer.error]] *)
let error s =
  raise (L.Error (spf "Lexical error: %s" s, !L.line))
(*e: function [[Lexer.error]] *)
(*s: function [[Lexer.loc]] *)
let loc () = !L.line
(*e: function [[Lexer.loc]] *)
(*s: function [[Lexer.inttype_of_suffix]] *)
let inttype_of_suffix sign size =
  let sign =
    match String.lowercase_ascii sign with
      | "" -> T.Signed
      | "u" -> T.Unsigned
      | s -> error (spf "Impossible: wrong sign suffix: %s" s)
  in
  match String.lowercase_ascii size with
  | "" -> T.Int, sign
  | "l" -> T.Long, sign
  | "ll" -> T.VLong, sign
  | s -> error (spf "Impossible: wrong int size suffix: %s" s)
(*e: function [[Lexer.inttype_of_suffix]] *)
(*s: function [[Lexer.floattype_of_suffix]] *)
let floattype_of_suffix s =
  match String.lowercase_ascii s with
  | "" -> T.Double
  | "f" -> T.Float
  | s -> error (spf "Impossible: wrong float size suffix: %s" s)
(*e: function [[Lexer.floattype_of_suffix]] *)
(* dup: lexer_asm5.mll *)
(*s: function [[Lexer.code_of_escape_char]] *)
let code_of_escape_char c =
  match c with
  | 'n' -> Char.code '\n' | 'r' -> Char.code '\r' 
  | 't' -> Char.code '\t' | 'b' -> Char.code '\b' 

  (* compatibility with plan 9 C code? *)
  | 'f' -> Logs.err (fun m -> m "unknown \\f"); 0x00
  (* could be removed, special 5c escape char *)
  | 'a' -> 0x07 | 'v' -> 0x0b 

  | '\\' | '\'' | '"' -> Char.code c 
  (* stricter: we disallow \ with unknown character *)
  | _ -> error "unknown escape sequence"
(*e: function [[Lexer.code_of_escape_char]] *)
(* dup: lexer_asm5.mll *)
(*s: function [[Lexer.string_of_ascii]] *)
let string_of_ascii i =
  String.make 1 (Char.chr i)
(*e: function [[Lexer.string_of_ascii]] *)
(*s: function [[Lexer.char_]] *)
(* needed only because of ocamllex limitations in ocaml-light
 * which does not support the 'as' feature.
 *)
let char_ lexbuf =
  let s = Lexing.lexeme lexbuf in
  String.get s 0
(*e: function [[Lexer.char_]] *)
(*e: Lexer helpers *)
}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
(*s: constant [[Lexer.space]] *)
let space = [' ''\t']
(*e: constant [[Lexer.space]] *)
(*s: constant [[Lexer.letter]] *)
let letter = ['a'-'z''A'-'Z']
(*e: constant [[Lexer.letter]] *)
(*s: constant [[Lexer.digit]] *)
let digit = ['0'-'9']
(*e: constant [[Lexer.digit]] *)
(*s: constant [[Lexer.oct]] *)
let oct = ['0'-'7']
(*e: constant [[Lexer.oct]] *)
(*s: constant [[Lexer.hex]] *)
let hex = (digit | ['A'-'F''a'-'f'])
(*e: constant [[Lexer.hex]] *)

(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)
(*s: rule [[Lexer.token]] *)
rule token = parse
  (* ----------------------------------------------------------------------- *)
  (* Spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token]] space/comment cases *)
  | space+        { token lexbuf }
  (*x: [[Lexer.token]] space/comment cases *)
  | "//" [^'\n']* { token lexbuf }
  | "/*"          { comment lexbuf }
  (*x: [[Lexer.token]] space/comment cases *)
  | '\n'          { incr Location_cpp.line; token lexbuf }
  (*e: [[Lexer.token]] space/comment cases *)

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token]] symbol cases *)
  | "+" { TPlus (loc()) } | "-" { TMinus (loc()) }
  | "*" { TMul  (loc()) } | "/" { TDiv   (loc()) } | "%" { TMod (loc()) }

  | "&"  { TAnd  (loc()) }  | "|"  { TOr     (loc()) } | "^" { TXor (loc()) }
  | "<<" { TInfInf (* TLsh *) (loc()) } | ">>" { TSupSup (* TRsh *) (loc()) }
  | "~"  { TTilde  (loc()) }

  | "&&" { TAndAnd (loc()) } | "||" { TOrOr (loc()) }
  | "!"  { TBang   (loc()) }

  | "++" { TPlusPlus (loc()) } | "--" { TMinusMinus (loc()) }

  | "="  { TEq   (loc()) }
  | "==" { TEqEq (loc()) }  | "!=" { TBangEq (loc()) }

  | "+=" { TOpEq (loc(), A.Plus) } | "-=" { TOpEq (loc(), A.Minus) }
  | "*=" { TOpEq (loc(), A.Mul) }  | "/=" { TOpEq (loc(), A.Div) }
  | "%=" { TOpEq (loc(), A.Mod) }
  | "&=" { TOpEq (loc(), A.And) }        | "|="  { TOpEq (loc(), A.Or) }
  | "^=" { TOpEq (loc(), A.Xor) }
  | ">>="{ TOpEq (loc(), A.ShiftRight) } | "<<=" { TOpEq (loc(), A.ShiftLeft) }

  | "<"  { TInf   (loc()) } | ">"  { TSup   (loc()) }
  | "<=" { TInfEq (loc()) } | ">=" { TSupEq (loc()) }
  (*x: [[Lexer.token]] symbol cases *)
  | "(" { TOPar   (loc()) } | ")" { TCPar   (loc()) }
  | "{" { TOBrace (loc()) } | "}" { TCBrace (loc()) }
  | "[" { TOBra   (loc()) } | "]" { TCBra   (loc()) }
           
  | ","  { TComma (loc()) } | ";"  { TSemicolon (loc()) }
  | "->" { TArrow (loc()) }
  | "."  { TDot   (loc()) }
  | "?"  { TQuestion (loc()) }
  | ":"  { TColon    (loc()) }
  (*e: [[Lexer.token]] symbol cases *)

  (* ----------------------------------------------------------------------- *)
  (* Numbers *)
  (* ----------------------------------------------------------------------- *)
  (* dup: lexer_asm5.mll *)
  (*s: [[Lexer.token]] number cases *)
  (*s: [[Lexer.token]] octal case *)
  | "0"  (oct+ (*as s*)) (['U''u']? (*as unsigned*)) (['L''l']* (*as long*))
      { let (s, unsigned, long) = 
           let s = Lexing.lexeme lexbuf in
           s =~ "0\\([0-7]+\\)\\([Uu]?\\)\\([Ll]*\\)" |> ignore;
           Regexp.matched3 s
        in
        TIConst(loc(), "0o" ^ s, inttype_of_suffix unsigned long)}
  (*e: [[Lexer.token]] octal case *)
  (*s: [[Lexer.token]] hexadecimal case *)
  | "0x" (hex+ (*as s*))  (['U''u']? (*as unsigned*)) (['L''l']* (*as long*))
      { let (s, unsigned, long) = 
           let s = Lexing.lexeme lexbuf in
           s =~ "0x\\([0-9A-Fa-f]+\\)\\([Uu]?\\)\\([Ll]*\\)" |> ignore;
           Regexp.matched3 s
        in
        TIConst(loc(), "0x" ^ s, inttype_of_suffix unsigned long)}
  (*e: [[Lexer.token]] hexadecimal case *)
  | "0x" { error "malformed hex constant" }

  (*s: [[Lexer.token]] decimal case *)
  | (['0'-'9'] digit*) (*as s*) (['U''u']? (*as unsigned*)) (['L''l']* (*as long*))
      { let (s, unsigned, long) = 
           let s = Lexing.lexeme lexbuf in
           s =~ "\\([0-9]+\\)\\([Uu]?\\)\\([Ll]*\\)" |> ignore;
           Regexp.matched3 s
        in
        TIConst (loc(), s, inttype_of_suffix unsigned long)}
  (*e: [[Lexer.token]] decimal case *)

  (*s: [[Lexer.token]] float case *)
  (* stricter: I impose some digit+ after '.' and after 'e' *)
  | ((digit+ | digit* '.' digit+) (['e''E'] ('+' | '-')? digit+)?) (*as s*)
      (['F''f']* (*as float*))
     { let (s, float) =
          let s = Lexing.lexeme lexbuf in
          s =~ "\\([^Ff]+\\)\\([Ff]*\\)$" |> ignore;
          Regexp.matched2 s
        in
        TFConst (loc(), s, floattype_of_suffix float) }
  (*e: [[Lexer.token]] float case *)
  (* special regexp for better error message *)
  | (digit+ | digit* '.' digit+) ['e''E'] ('+' | '-')?
     { error "malformed fp constant exponent" }
  (*e: [[Lexer.token]] number cases *)

  (* ----------------------------------------------------------------------- *)
  (* Chars/Strings *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token]] chars/strings cases *)
  (* converting characters in integers *)
  | "'" { TIConst (loc(), spf "%d" (char lexbuf), (T.Char, T.Signed)) }
  (*x: [[Lexer.token]] chars/strings cases *)
  | '"' { TString (loc(), string lexbuf, T.Array (None, T.I (T.Char,T.Signed)))}
  (*e: [[Lexer.token]] chars/strings cases *)

  (* ----------------------------------------------------------------------- *)
  (* Keywords and identifiers *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token]] keywords/identifiers cases *)
  | (letter | '_') (letter | digit | '_')* {
      let s = Lexing.lexeme lexbuf in

      match s with
      | "static" -> Tstatic (loc()) 
      | "extern" -> Textern (loc())
      (* we could forbid those constructs; they are not really useful *)
      | "auto" -> Tauto (loc()) 
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
        (*s: [[Lexer.token()]] in identifier case, typedef trick *)
        if Hashtbl.mem Globals.hids s
        then 
          (* typedef trick, because ambiguity in C grammar *)
          (match Hashtbl.find Globals.hids s with
          | A.IdIdent | A.IdEnumConstant -> TName (loc(), s)
          | A.IdTypedef -> TTypeName (loc(), s)
          )
        (*e: [[Lexer.token()]] in identifier case, typedef trick *)
        else TName (loc(), s)
  }
  (*e: [[Lexer.token]] keywords/identifiers cases *)

  (* ----------------------------------------------------------------------- *)
  (* CPP *)
  (* ----------------------------------------------------------------------- *)
  (*s: [[Lexer.token]] cpp cases *)
  (* See ../macroprocessor/lexer_cpp.mll *)
  | "#" { TSharp }
  (*e: [[Lexer.token]] cpp cases *)

  (* ----------------------------------------------------------------------- *)
  | eof { EOF }
  | _ (*as c*)   { let c = char_ lexbuf in
                   error (spf "unrecognized character: '%c'" c) }
(*e: rule [[Lexer.token]] *)

(*****************************************************************************)
(* String rule *)
(*****************************************************************************)
(*s: rule [[Lexer.string]] *)
and string = parse
  | '"' { "" }
  | "\\" ((oct oct oct) (*as s*))
      { let s = Lexing.lexeme lexbuf |> String_.drop_prefix 1 in
        let i = int_of_string ("0o" ^ s) in string_of_ascii i ^ string lexbuf }
  | "\\" (['a'-'z' '\\' '"'] (*as c*)) 
      { let c = String.get (Lexing.lexeme lexbuf) 1 in
        let i = code_of_escape_char c in string_of_ascii i ^ string lexbuf  }
  (* strings can contain newline! but they must be escaped before *)
  | '\\' '\n' { "\n" ^ string lexbuf }
  | [^ '\\' '"' '\n']+   
      { let x = Lexing.lexeme lexbuf in x ^ string lexbuf }
  | '\n' { error "newline in string" }
  | eof  { error "end of file in string" }
  | _    { error "undefined character in string" }
(*e: rule [[Lexer.string]] *)

(*****************************************************************************)
(* Character rule *)
(*****************************************************************************)
(*s: rule [[Lexer.char]] *)
and char = parse
  | "''"                            { Char.code '\'' }
  (* less: 5c allows up to 8 octal number when in L'' mode *)
  | "\\" ((oct oct? oct?) (*as s*)) "'" 
      { let s = Lexing.lexeme lexbuf |>
           String_.drop_prefix 1 |> String_.drop_suffix 1
        in
        int_of_string ("0o" ^ s) }
  | "\\" (['a'-'z' '\\' '\''] (*as c*)) "'"       
      { let c = String.get (Lexing.lexeme lexbuf) 1 in
        code_of_escape_char c }
  | '\\' '\n' { char lexbuf }
  | [^ '\\' '\'' '\n'] (*as c*)  "'"    
      { let c = String.get (Lexing.lexeme lexbuf) 0 in
        Char.code c }
  | '\n' { error "newline in character" }
  | eof  { error "end of file in character" }
  | _    { error "missing '" }
(*e: rule [[Lexer.char]] *)

(*****************************************************************************)
(* Comment rule *)
(*****************************************************************************)
(* dup: lexer_asm5.mll *)
(*s: rule [[Lexer.comment]] *)
and comment = parse
  | "*/"          { token lexbuf }
  | [^ '*' '\n']+ { comment lexbuf }
  | '*'           { comment lexbuf }
  | '\n'          { incr Location_cpp.line; comment lexbuf }
  | eof           { error "eof in comment" }
(*e: rule [[Lexer.comment]] *)
(*e: Lexer.mll *)
