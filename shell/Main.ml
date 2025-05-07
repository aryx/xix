(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of rc, the Plan 9 shell.
 *
 * Main limitations compared to rc:
 *  - no unicode support
 *  - not all of the fancy redirections and fancy pipes
 *  - no storing of functions in the environment
 *    (used by rcmain. But can do the same by using '. rcmain')
 * 
 * Improvements (IMHO):
 *  - a strict mode where we report when deleting undefined function
 *  - more?
 * 
 * todo:
 *  - read environment variables and export variables
 *  - globbing
 *  - Isatty rc -i detection
 *  - add ~ shortcut for HOME (from csh?)
 *  - rc -c
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let _ = 
    Cap.main CLI.main
