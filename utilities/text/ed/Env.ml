(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
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
  mutable zero : file_offset array;
  (* index in zero *)
  mutable dot: lineno;
  mutable dol: lineno;
  (* alt: separate "range" type *)
  mutable addr1: lineno;
  mutable addr2: lineno;

  (* can't use {in/out}_channel because we need to both read and write
   * the temporary file, hence the use of the more general Unix.file_descr
   *)
  tfile : Unix_.file_descr;

  mutable savedfile: Fpath.t;

  vflag: bool;
  (* used just in putchr() so could be removed almost *)
  oflag: bool;

  (* to read the commands from *)
  stdin: Lexing_.lexbuf;
  (* stdout unless oflag is set in which case it's stderr *)
  out: out_channel [@printer fun fmt _ -> 
        Format.fprintf fmt "<out_channel>"];
}
[@@deriving show]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let init (caps : < Cap.stdin; ..>) (vflag : bool) (oflag : bool) : env =
  { zero = Array.make 10 0;
    dot = 0;
    dol = 0;
    addr1 = 0;
    addr2 = 0;

    tfile = Unix.openfile !!tfname [ Unix.O_RDWR; Unix.O_CREAT ] 0o600;
    vflag = if oflag then false else vflag;
    oflag;
    savedfile = if oflag then Fpath.v "/fd/1" else Fpath.v "NOFILE";
    stdin = Lexing.from_channel (Console.stdin caps);
    out = if oflag then Console.stderr caps else Console.stdout caps;
  }

(* TODO? type cmd = Read of range | ... ? *)
