(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

module D = Ast_cpp (* D for Directives *)
module L = Location_cpp
module Flags = Flags_cpp

(* for ocaml-light for fields *)
open Ast_cpp 
open Preprocessor

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Cpp as a library. This is used by 5c but also 5a.
 * 
 * Main limitations compared to the cpp embedded in 5c of Plan 9:
 *  - no support for unicode
 *  - see Lexer_cpp.mll
 * Main limitations compared to ANSI cpp:
 *  - no complex boolean expressions for #ifdefs
 *
 * stricter:
 *  - see Lexer_cpp.mll
 * more general:
 *  - allow any number of arguments for macros 
 *    (not limited to 25 because of the use of #a to #z)
 *  - allow any body size 
 *    (no 8196 buffer limit)
 *  - allow any filename length in #include 
 *    (no 200 limit)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type token_category =
  | Eof
  | Sharp
  | Ident of string
  | Other
[@@deriving show]

type ('token, 'ast) hook = {
  lexer: Lexing.lexbuf -> 'token;
  parser: (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'ast;
  category: 'token -> token_category;
  (* TODO: delete, redundant with Eof token_category above *)
  eof: 'token;
}

(* similar to Ast_cpp.macro *)
type macro = {
  m_name: string;
  m_nbargs: int option;
  m_varargs: bool; (* use "..." *)

  (* body contains #xxx substrings corresponding to the parameter of the macro.
   * For instance, #define foo(a,b) a+b --> {name="foo";nbargs=2;body="#1+#2"}.
   * 
   * Is there a risk of having numbers squashed with the macro parameter?
   * No, because if you have 'a1+b' then 'a1' is a separate identifier 
   * so you can not generate #11+#2 .
   *)
  m_body: string;
}

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let hmacros = Hashtbl.create 101

(* cwd is used to manage #include "...". It is altered when you
 * include a file. cwd becomes the dirname of the included file??? 
 * TODO: dead? used?
 *)
let _cwd = ref (Sys.getcwd ())

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s =
  raise (L.Error (s, !L.line))


let define_cmdline_def (k, v) =
  Hashtbl.add hmacros k 
    { m_name = k; m_nbargs = None; m_varargs = false; m_body = v; }

let define {name = s; params; varargs; body}=
  (* We could forbid here 's' to conflict with C keyboard, but this
   * should be done in the caller, as cpp can be used with different
   * languages, which may use different keywords.
   *)
  let sbody = match body with Some x -> x | None -> "1" in
  if Hashtbl.mem hmacros s
  then error (spf "macro redefined: %s" s)
  else begin
    let macro =
      match params with
      | None -> 
          { m_name = s; m_nbargs = None; m_varargs = false; m_body = sbody }
      | Some params ->
          { m_name = s; m_nbargs = Some (List.length params); 
            m_varargs = varargs; m_body = sbody }
    in
    if !Flags_cpp.debug_macros
    then Logs.app (fun m -> m "#define %s %s" s macro.m_body);

    Hashtbl.add hmacros s macro
  end


(* less: Could use Set instead of list for the set of include paths 
 * todo: if absolute path, need to find it.
*)
let rec find_include (conf : Preprocessor.conf) (f : Fpath.t) (system : bool) =
  if system
  then find_include_bis conf.paths f
  else find_include_bis (conf.dir_source_file::conf.paths) f
and find_include_bis (paths : Fpath.t list) (f : Fpath.t) : Fpath.t =
  match paths with 
  (* stricter: better error message *)
  | [] -> failwith (spf "could not find %s in include paths" !!f)
  | x::xs ->
      let path : Fpath.t = 
        if !!x = "."
        (* this will handle also absolute path *)
        then f
        else x // f 
      in
      if Sys.file_exists !!path
      then begin
        if !Flags.debug_inclusion
        then Logs.app (fun m -> m "%d: %s" !L.line !!path);
        path
      end
      else find_include_bis xs f

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse (caps : < Cap.open_in; ..>) hooks (conf : Preprocessor.conf) (file : Fpath.t) = 
  L.history := [];
  L.line := 1;
  Hashtbl.clear hmacros;
  conf.defs |> List.iter define_cmdline_def;

  let chan = CapStdlib.open_in caps !!file in
  Logs.info (fun m -> m "opening %s" !!file);

  L.add_event (L.Include file);
  let lexbuf = Lexing.from_channel chan in
  let stack = ref [(Some chan, lexbuf)] in
  (* less: let push x = check if too deep? *)

  (* for more precise error reporting *)
  let last_ident = ref "" in

  let rec lexfunc () =
    match !stack with
    | (chanopt, lexbuf)::xs ->
        let t = hooks.lexer lexbuf in
        let categ = hooks.category t in

        (match categ with
        | Eof -> 
            stack := xs;
            chanopt |> Option.iter (fun chan ->
              close_in chan;
              L.add_event L.Eof;
            );
            lexfunc ()

        | Sharp ->

            (* treating cpp directives *)
            let t = Lexer_cpp.token lexbuf in
            (match t with
            | D.Include (f, system_hdr) ->
                let path = find_include conf f system_hdr in
                (try 
                  let chan = CapStdlib.open_in caps !!path in
                  Logs.info (fun m -> m "opening included file %s" !!path);
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
            | D.Define macro_ast ->
               (* todo: stricter: forbid s to conflict with C keyboard *)
                define macro_ast
            | D.Undef s ->
                (* stricter: check that was defined *)
                if not (Hashtbl.mem hmacros s)
                then error (spf "macro %s was not defined" s);
                Hashtbl.remove hmacros s
            | D.Line (line, file) ->
                L.add_event (L.Line (line, file));
            (* less: for "lib" should add a L.PragmaLib event? *)
            | D.Pragma _ -> ()
            | D.Ifdef s ->
                if Hashtbl.mem hmacros s
                then ()
                else Lexer_cpp.skip_for_ifdef 0 true lexbuf
            | D.Ifndef s ->
                if not (Hashtbl.mem hmacros s)
                then ()
                else Lexer_cpp.skip_for_ifdef 0 true lexbuf
            | D.Else ->
                Lexer_cpp.skip_for_ifdef 0 true lexbuf
            | D.Endif -> ()
            );
            lexfunc ()

        | Ident s ->
            last_ident := s;
            if Hashtbl.mem hmacros s
            then 
              let macro = Hashtbl.find hmacros s in
              match macro.m_nbargs with
              | None ->
                  let body = macro.m_body in
                  if !Flags_cpp.debug_macros
                  then Logs.app (fun m -> m "#expand %s %s" s body);
                  let lexbuf = Lexing.from_string body in
                  stack := (None, lexbuf)::!stack;
                  lexfunc ()
              | Some n ->
                  let args = Lexer_cpp.macro_arguments lexbuf in
                  if List.length args <> n
                  then error (spf "argument mismatch expanding: %s" s)
                  else begin
                    let body = macro.m_body in
                    let lexbuf = Lexing.from_string body in
                    let body = 
                      Lexer_cpp.subst_args_in_macro_body s args lexbuf in
                    if !Flags_cpp.debug_macros
                    then Logs.app (fun m -> m "#expand %s %s" s body);
                    let lexbuf = Lexing.from_string body in
                    stack := (None, lexbuf)::!stack;
                    lexfunc ()
                  end
            else t
        | _ -> t
        )
    (* no more stack, could raise Impossible instead? *)           
    | [] -> hooks.eof
  in

  (try 
    hooks.parser (fun _lexbuf -> lexfunc ()) lexbuf
  with Parsing.Parse_error ->
    error ("Syntax error" ^ 
              (if !last_ident = "" 
               then ""
               else spf ", last name: %s" !last_ident))
  )
