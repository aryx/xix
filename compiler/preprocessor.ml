(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Main limitations compared to the cpp embedded in 5c of Plan 9:
 *  - see lexer_cpp.mll
 * Limitations compared to ANSI cpp:
 *  - no complex boolean expressions for #ifdefs
 * 
 * stricter:
 *  - 
 * more general:
 *  - allow any number of arguments for macros 
 *    (not limited to 25 because of the use of #a to #z)
 *  - allow any body size 
 *    (no 8196 buffer limit)
 *  - allow any filename size in include 
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

  (* body contains #xxx substrings corresponding to the parameter of the macro.
   * For instance, #define foo(a,b) a+b --> {name="foo";nbargs=2;body="#1+#2"}.
   * Do we have a risk of having numbers squashed with the macro parameter?
   * No, because if you have 'a1+b' then 'a1' is a separate identifier 
   * so you can't generate #11+#2 .
   *)
  body: string;
}

type directive =
  | Include of Common.filename * bool (* true if <>, false if "" *)

  | Line of int * Common.filename

  | Define of macro
  | Undef of string

  | Ifdef of string
  | Ifndef of string
  | Else
  | Endif

  | Pragma of string * string


type line_history = HistoryTodo

(*
exception Error of string * int
*)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* cwd is used to manage #include "..."; It is altered when you
 * include a file; cwd becomes the dirname of the included file??? *)
let cwd = ref (Sys.getcwd ())

(* We could have the global line below defined here instead of in globals.ml. 
 * However, we can also call the C parser after cpp, in which
 * case the parser has nothing to do with the preprocessor,
 * but the parser still needs to manage a line number, so better to put
 * line in globals.ml.
 * 
 * let line = ref 1
 *)


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*
 * let final_location_of_lineno lineno =
 *  raise Todo
 * 
 * let find_included_file file include_paths =
 *  raise Todo
 *)

