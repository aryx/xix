(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module L = Location_cpp
module Flags = Flags_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Cpp as a library.
 * This is used by 5c but also 5a.
 * 
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
type system_paths = Common.filename list

(* The first element in the list is supposed to contain the directory
 * of the C file so it is looked for "" but not for <>
 *)
type include_paths = Common.filename * system_paths


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
  let sbody = match body with Some x -> x | None -> "1" in
  if Hashtbl.mem hmacros s
  then raise (L.Error (spf "macro redefined: %s" s, !L.line))
  else begin
    let macro =
      match params with
      | None -> 
          { name = s; nbargs = None; varargs = false; body = sbody }
      | Some (params, varargs) ->
          { name = s; nbargs = Some (List.length params); 
            varargs = varargs; body = sbody }
    in
    if !Flags_cpp.debug_macros
    then pr2 (spf "#define %s %s" s macro.body);

    Hashtbl.add hmacros s macro
  end


(* less: Could use Set instead of list for the set of include paths *)
let rec find_include (dir, system_paths) (f, system) =
  if system
  then find_include_bis system_paths f
  else find_include_bis (dir::system_paths) f
and find_include_bis paths f =
  match paths with 
  (* stricter: better error message *)
  | [] -> failwith (spf "could not find %s in include paths" f)
  | x::xs ->
      let path = Filename.concat x f in
      if Sys.file_exists path
      then begin
        if !Flags.debug_inclusion
        then pr (spf "%d: %s" !L.line path);
        path
      end
      else find_include_bis xs f
