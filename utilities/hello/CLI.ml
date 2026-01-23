(* Copyright 2026 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
module Str = Re_str

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Toy hello program acting as a template for new projects in xix hello.
 *
 * Limitations compared to Plan 9 version:
 *  - no unicode (runes) support
 *  - lots of missing features
 *
 * Improvements over Plan 9 C version:
 *  - clearer error messages (via logging)
 *  - far less globals!
*)

(*****************************************************************************)
(* Caps *)
(*****************************************************************************)
type caps = < Cap.stdout >

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let hello (caps : < caps; ..>) =
  Console.print caps "Hello"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : <caps; Cap.stderr; ..>) (argv : string array) : Exit.t =

  let args = ref [] in
  let level = ref (Some Logs.Warning) in

  let options = [
     "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose logging mode";
     "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
     " debug logging mode";
     "-quiet", Arg.Unit (fun () -> level := None),
     " quiet logging mode";

  ] |> Arg.align
  in
  (* may raise ExitCode *)
  Arg_.parse_argv caps argv options (fun t -> args := t::!args) 
    (spf "usage: %s [options] [file]" argv.(0));
  Logs_.setup !level ();

  (match !args with
  | [] -> hello caps
  | _ -> 
     failwith "too many arguments" 
  );
  Exit.OK
