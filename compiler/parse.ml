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

let error s =
  raise (L.Error (s, !L.line))

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

  (* for more precise error reporting *)
  let last_ident = ref "" in

  let rec lexfunc () =
    match !stack with
    | (chanopt, lexbuf)::xs ->
        let t = Lexer.token lexbuf in

        (match t with
        | T.EOF -> 
            stack := xs;
            chanopt |> Common.if_some (fun chan ->
              close_in chan;
              L.add_event L.Eof;
            );
            lexfunc ()

        | T.TSharp ->
            let t = Lexer_cpp.token lexbuf in
            (match t with
            | D.Include (f, system_hdr) ->
                let path = Preprocessor.find_include paths (f, system_hdr) in
                (try 
                  let chan = open_in path in
                  L.add_event (L.Include path);
                  let lexbuf = Lexing.from_channel chan in
                  (* less: 
                     if List.length stack > 1000
                     then error "macro/io expansion too deep"
                  *)
                  stack := (Some chan, lexbuf)::!stack;
                with Failure s ->
                  error s
                )

            | D.Define (s, params, body) ->
               (* todo: stricter: forbid s to conflict with C keyboard *)
                Preprocessor.define (s, params, body)

            | D.Undef s ->
                (* stricter: check that was defined *)
                if not (Hashtbl.mem Preprocessor.hmacros s)
                then error (spf "macro %s was not defined" s);
                Hashtbl.remove Preprocessor.hmacros s

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

        | T.TName _ | T.TTypeName _ 
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
          ->
            let s = Lexing.lexeme lexbuf in
            last_ident := s;
            if Hashtbl.mem Preprocessor.hmacros s
            then 
              let macro = Hashtbl.find Preprocessor.hmacros s in
              match macro.P.nbargs with
              | None ->
                  let body = macro.P.body in
                  if !Flags_cpp.debug_macros
                  then pr2 (spf "#expand %s %s" s body);
                  let lexbuf = Lexing.from_string body in
                  stack := (None, lexbuf)::!stack;
                  lexfunc ()
              | Some n ->
                  let args = Lexer_cpp.macro_arguments lexbuf in
                  if List.length args <> n
                  then error (spf "argument mismatch expanding: %s" s)
                  else begin
                    raise Todo
                  end
            else t
        | _ -> t
        )
    (* no more stack, could raise Impossible instead? *)           
    | [] -> T.EOF
  in

  (try 
    Parser.prog (fun _lexbuf -> lexfunc ()) lexbuf
  with Parsing.Parse_error ->
    error ("Syntax error" ^ 
              (if !last_ident = "" 
               then ""
               else spf ", last name: %s" !last_ident))
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
