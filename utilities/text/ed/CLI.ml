(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Eq.Operators
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of ed of Unix and Plan 9.
 *
 * Limitations compared to Plan9 version:
 *  - no unicode (runes) support
 *
 * Improvements over Plan 9 C version:
 *  - far less globals
 *  - no fixed-size array for saved file, buffers, lines, etc.
*)

type caps = < Cap.stdin; Cap.stdout; Cap.open_in; Cap.open_out >

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

(* LATER: use Tmp.with_new_file *)
let tfname = Fpath.v "/tmp/oed.scratch"

(* offset in tfname file content *)
type file_offset = int
[@@deriving show]

(* ed uses 1-indexed line numbers, but 0 is also used as a special value *)
type lineno = int
[@@deriving show]

type env = {

  (* growing array of line offsets in tfname. 1-indexed array but the 0
   * entry is used as a sentinel.
   *)
  zero : file_offset array;
  (* index in zero *)
  mutable dot: lineno;
  mutable dol: lineno;
  (* alt: separate "range" type *)
  mutable addr1: lineno;
  mutable addr2: lineno;

  (* can't use {in/out}_channel because we need to both read and write
   * the temporary file
   *)
  tfile : Unix_.file_descr;

  mutable savedfile: Fpath.t;

  vflag: bool;
  (* used just in putchr() so could be removed almost *)
  oflag: bool;
}
[@@deriving show]

let init (vflag : bool) (oflag : bool) : env =
  { zero = Array.make 10 0;
    dot = 0;
    dol = 0;
    addr1 = 0;
    addr2 = 0;

    tfile = Unix.openfile !!tfname [ Unix.O_RDWR; Unix.O_CREAT ] 0o644;
    vflag = if oflag then false else vflag;
    oflag;
    savedfile = if oflag then Fpath.v "/fd/1" else Fpath.v "NOFILE";
  }

(* TODO? type cmd = Read of range | ... ? *)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (_caps : <caps; ..>) (argv : string array) : Exit.t =
  let args = ref [] in
  let level = ref (Some Logs.Warning) in
  let vflag = ref true in
  let oflag = ref false in
  let options = [
     "-", Arg.Clear vflag,
     " reset verbose mode";
     "-o", Arg.Set oflag,
     " output to standard output (instead of editing a file)";

     (* new: *)
     "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
  ] |> Arg.align
  in
  (try 
    Arg.parse_argv argv options (fun t -> args := t::!args) 
      (spf "usage: %s [-lwc] [file ...]" argv.(0));
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  (* alt: use Arg and process -debug, -verbose, etc. *)
  Logs_.setup !level ();

  let env = init !vflag !oflag in
  let env =
    match List.rev !args with
    | [] -> env
    | [file] -> { env with savedfile = Fpath.v file }
    | _::_::_ -> failwith "too many arguments" 
  in
  (* ed: was globp *)
  let _first_command =
    if !oflag then 'a' else 'r'
  in
  Logs.info (fun m -> m "env = %s" (show_env env));
  
  Exit.OK