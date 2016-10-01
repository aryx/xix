(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module T = Parser  (* T for Tokens *)
module D = Ast_cpp (* D for Directives *)
module L = Location_cpp
module P = Preprocessor

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse (defs, paths) file = 

  L.line := 1;
  defs |> List.iter Preprocessor.define_cmdline_def;

  let chan = open_in file in
  L.add_event (L.Include file);
  let lexbuf = Lexing.from_channel chan in
  let stack = ref [(Some chan, lexbuf)] in
  (* less: let push x = check if too deep? *)

  let last_ident = ref "" in

  let rec lexfunc () =
    match !stack with
    | (chanopt, lexbuf)::xs ->
        let t = Lexer.token lexbuf in

        (match t with
        | T.EOF -> 
            stack := xs;
            (match chanopt with
            | Some chan ->
                close_in chan;
                L.add_event L.Eof; (* TODO: unless was macro expansion? *)
            | None -> ()
            );
            lexfunc ()
        (* less: 
           if List.length stack > 1000
           then error "macro/io expansion too deep"
        *)

        | T.TSharp ->
            let t = Lexer_cpp.token lexbuf in
            (match t with
            | D.Include (f, system) ->
                let path = Preprocessor.find_include paths (f, system) in
                (try 
                  let chan = open_in path in
                  L.add_event (L.Include path);
                  let lexbuf = Lexing.from_channel chan in
                  stack := (Some chan, lexbuf)::!stack;
                with Failure s ->
                  raise (L.Error (s, !L.line))
                )

            | D.Define (s, params, body) ->
                Preprocessor.define (s, params, body)

            | D.Undef s ->
                (* stricter: check that was defined *)
                if not (Hashtbl.mem Preprocessor.hmacros s)
                then raise (L.Error (spf "macro %s was not defined" s,!L.line))
                else Hashtbl.remove Preprocessor.hmacros s

            | D.Line (line, file) ->
                L.add_event (L.Line (line, file));

            (* less: for "lib" should add a L.PragmaLib event? *)
            | D.Pragma _ -> ()

            | D.Ifdef _
            | D.Ifndef _
            | D.Else
            | D.Endif
                -> raise Todo
            );
            lexfunc ()

        (* stricter: in theory could do macro for reserved keyword? *)
        | T.TName _ | T.TTypeName _ 
        | T.Tvoid | T.Tchar | T.Tshort | T.Tint | T.Tlong
        | T.Tdouble | T.Tfloat | T.Tsigned | T.Tunsigned
        | T.Tstruct | T.Tunion | T.Tenum | T.Ttypedef
        | T.Tconst | T.Tvolatile | T.Trestrict | T.Tinline
        | T.Tauto | T.Tstatic | T.Textern | T.Tregister
        | T.Tif | T.Telse | T.Twhile | T.Tdo | T.Tfor 
        | T.Tbreak | T.Tcontinue | T.Treturn | T.Tgoto
        | T.Tswitch | T.Tcase | T.Tdefault | T.Tsizeof
          ->
            let s = Lexing.lexeme lexbuf in
            last_ident := s;
            if Hashtbl.mem Preprocessor.hmacros s
            then 
              let macro = Hashtbl.find Preprocessor.hmacros s in
              if macro.P.nbargs = None
              then begin
                let body = macro.P.body in
                if !Flags_cpp.debug_macros
                then pr2 (spf "#expand %s %s" macro.P.name body);
                let lexbuf = Lexing.from_string body in
                stack := (None, lexbuf)::!stack;
                lexfunc ()
              end else begin
                raise Todo
              end
            else t
        | _ -> t
        )
           
    | [] -> T.EOF
  in

  (try 
    Parser.prog (fun _lexbuf -> lexfunc ()) lexbuf
  with Parsing.Parse_error ->
    (* less: record last name, and do like 5c *)
    raise (L.Error ((if !last_ident = "" 
                    then "Syntax error" 
                    else spf "Syntax error, last name: %s" !last_ident)
                   , !L.line))
  )



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
