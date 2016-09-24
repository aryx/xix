(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Main limitations compared to the cpp embedded in 5c of Plan 9:
 *  - no support for unicode
 *  - see lexer_cpp.mll
 * Main limitations compared to ANSI cpp:
 *  - no complex boolean expressions for #ifdefs
 * 
 * stricter:
 *  - 
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

(* -D *)
type cmdline_defs = (string * string) list

(* -I *)
type include_paths = Common.filename list

type macro = {
  name: string;
  nbargs: int option;
  varargs: bool; (* use "..." *)

  (* body contains #xxx substrings corresponding to the parameter of the macro.
   * For instance, #define foo(a,b) a+b --> {name="foo";nbargs=2;body="#1+#2"}.
   * 
   * Is there a risk of having numbers squashed with the macro parameter?
   * No, because if you have 'a1+b' then 'a1' is a separate identifier 
   * so you can not generate #11+#2 .
   *)
  body: string;
}


(*
exception Error of string * int
*)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let hmacros = Hashtbl.create 101

(* cwd is used to manage #include "...". It is altered when you
 * include a file. cwd becomes the dirname of the included file??? *)
let cwd = ref (Sys.getcwd ())

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let define_cmdline_def (k, v) =
  Hashtbl.add hmacros k { name = k; nbargs = None; varargs = false; body = v; }

let define (s, params, body) =
  if Hashtbl.mem hmacros s
  then failwith (spf "macro redefined: %s" s)
  else 
    Hashtbl.add hmacros s
      (match params with
        | None -> 
          { name = s; nbargs = None; varargs = false; body = s }
        | Some (params, varargs) ->
          { name = s; nbargs = Some (List.length params); 
            varargs = varargs; body = s }
      )


(* less: Could use Set instead of list for the set of include paths *)
let rec find_include paths (f, system) =
  match paths with 
  (* stricter: better error message *)
  | [] -> failwith (spf "could not find %s in include paths" f)
  | x::xs ->
      if x = "." && system
      then find_include xs (f, system)
      else 
        let path = Filename.concat x f in
        if Sys.file_exists path
        then begin
          if !Flags.debug_inclusion
          then pr (spf "%d: %s" !L.line path);
          path
        end
        else find_include xs (f, system)
