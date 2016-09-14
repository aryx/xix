{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Parser

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to 5c:
 *  - no unicode support
 *  - handles just the #line directive, assumes external cpp
 *    (but better to factorize code and separate concerns anyway)
*)


(* less: do like prfile? but then need a lines_directives global? *)
let error s =
  failwith (spf "Lexical error: %s (line %d)" s !Globals.line)

}


(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']


(*****************************************************************************)
(* Main rule *)
(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* Spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | [' ''\t']+ { token lexbuf }

  | "//" [^'\n']* { token lexbuf }

  | "/*"          { comment lexbuf }

  | '\n' { incr Globals.line; token lexbuf }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  | "+" { TPlus } | "-" { TMinus }
  | "*" { TMul } | "/" { TDiv } | "%" { TMod }

  | "!" { TBang } | "!=" { TBangEq }
  | "&" { TAnd } | "|"   { TOr } | "^" { TXor }
  | "&&" { TAndAnd } | "||" { TOrOr }

  | "++" { TPlusPlus } | "--" { TMinusMinus }  

  | "<"  { TInf } | ">" { TSup }
  | "<=" { TInfEq } | ">=" { TSupEq }
  | "<<" { TInfInf (* TLsh *) } | ">>" { TSupSup (* TRsh *) }

  | "=" { TEq } | "==" { TEqEq }

  | "+=" { TOpEq "+") } | "-=" { TOpEq "-" } | "%=" { TOpEq "%" }
  | "*=" { TOpEq "*" }  | "/=" { TOpEq "/" } 
  | ">>=" { TOpEq ">>" } | "<<=" { TOpEq "<<" }
  | "&=" { TOpEq "&" } | "|=" { TOpEq "|" } | "^=" | { TOpEq "^" }

             

  | "->" { TArrow }

  (* ----------------------------------------------------------------------- *)
  (* Numbers *)
  (* ----------------------------------------------------------------------- *)
  (* dup: lexer_asm5.mll *)
  | "0"  (oct+ as s) (['U''u']? as sign) (['L''l'] as vlong)
      { TConst ("0o" ^ s, 
                if sign = "" then Signed else Unsigned, 
                  VLong)  }
  | "0x" hex+        
      { TConst (Lexing.lexeme lexbuf, 
                if sign = "" then Signed else Unsigned, 
                VLong) }

  (* stricter: I impose some digit+ after '.' and after 'e' *)
  | (digit+ | digit* '.' digit+) (['e''E'] ('+' | '-')? digit+)?
     { TFConst (Lexing.lexeme lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Strings and chars *)
  (* ----------------------------------------------------------------------- *)

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

      | "if" -> Tif | "else" -> Telse | "while" -> Twhile | "do" -> Tdo
      | "for" -> Tfor | "break" -> Tbreak | "continue" -> Tcontinue
      | "switch" -> Tswitch | "case" -> Tcase | "default" -> Tdefault
      | "return" -> Treturn | "goto" -> Tgoto

      | "sizeof" -> Tsizeof

      | _ -> 
        if Hashtbl.mem Globals.htypedefs s
        then TTypeName s
        else TName s
  }

  (* ----------------------------------------------------------------------- *)
  (* CPP *)
  (* ----------------------------------------------------------------------- *)
  (* stricter: I impose a filename (with no quote in name, hmm) 
   * less: normalize? realpath? 
   * dup: lexer_asm5.mll
   *)
  | "#line" space+ (digit+ as s1) space* ('"' ([^'"']* as s2) '"') 
      { TSharpLine (int_of_string s1, s2) }
  | "#line" { error "syntax in #line" }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF }
  | _ as c   { error (spf "unrecognized character: '%c'" c) }

(*****************************************************************************)
(* String rule *)
(*****************************************************************************)

(*****************************************************************************)
(* Character rule *)
(*****************************************************************************)

(*****************************************************************************)
(* Comment rule *)
(*****************************************************************************)
(* dup: lexer_asm5.mll *)
and comment = parse
  | "*/"          { token lexbuf }
  | [^ '*' '\n']+ { comment lexbuf }
  | '*'           { comment lexbuf }
  | '\n'          { incr Globals.line; comment lexbuf }
  | eof           { error "end of file in comment" }
