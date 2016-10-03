(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp
module T = Parser  (* T for Tokens *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (defs, paths) file = 
  let hooks = { Parse_cpp.
     lexer = Lexer.token;
     category = (fun t ->
       match t with
       | T.EOF -> Parse_cpp.Eof
       | T.TSharp -> Parse_cpp.Sharp

       | T.TName s | T.TTypeName s -> Parse_cpp.Ident s
        (* stricter: I forbid to have macros overwrite keywords *)
        (*
        | T.Tvoid | T.Tchar | T.Tshort | T.Tint | T.Tlong
        | T.Tdouble | T.Tfloat | T.Tsigned | T.Tunsigned
        | T.Tstruct | T.Tunion | T.Tenum | T.Ttypedef
        | T.Tconst | T.Tvolatile | T.Trestrict | T.Tinline
        | T.Tauto | T.Tstatic | T.Textern | T.Tregister
        | T.Tif | T.Telse | T.Twhile | T.Tdo | T.Tfor 
        | T.Tbreak | T.Tcontinue | T.Treturn | T.Tgoto
        | T.Tswitch | T.Tcase | T.Tdefault | T.Tsizeof
        *)
       | _ -> Parse_cpp.Other
     );
   parser = Parser.prog;
   eof = T.EOF;
  }
  in
  Parse_cpp.parse hooks (defs, paths) file



let parse_no_cpp file =
  file |> Common.with_file_in (fun chan ->
    L.line := 1;
    let lexbuf = Lexing.from_channel chan in
    (try 
      Parser.prog Lexer.token lexbuf
    with Parsing.Parse_error ->
      failwith (spf "Syntax error: line %d" !L.line)
    )
  )

