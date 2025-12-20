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

type t = {
  (* This is the temporary tfname file. We can't use the OCaml usual
   * {in/out}_channel type because we need to both read and write in
   * the temporary file, hence the use of the more general Unix.file_descr.
   *)
  tfile : Unix_.file_descr;

  (* to read the commands from *)
  stdin: Lexing_.lexbuf;
  (* stdout unless oflag is set in which case it's stderr *)
  out: out_channel [@printer fun fmt _ -> 
        Format.fprintf fmt "<out_channel>"];

  (* growing array of line offsets in tfile. 1-indexed array but the 0
   * entry is used as a sentinel.
   *)
  mutable zero : file_offset array;
  (* index in zero *)
  mutable dot: lineno;
  mutable dol: lineno;
  (* alt: separate "range" type *)
  mutable addr1: lineno;
  mutable addr2: lineno;

  (* for w, r, f *)
  mutable savedfile: Fpath.t option;
  mutable fchange: bool;
  mutable count: int;
  (* set by ? effect is to printcom() *)
  mutable pflag: bool;

  vflag: bool;
  (* used just in putchr() so could be removed almost *)
  oflag: bool;
}
[@@deriving show]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let init (caps : < Cap.stdin; ..>) (vflag : bool) (oflag : bool) : t =
  let out = if oflag then Console.stderr caps else Console.stdout caps in
  (* will be overwritten possibly in the caller by argv[1] *)
  let savedfile = if oflag then Some (Fpath.v "/fd/1") else None in
  { 
    tfile =
      (try
        Unix.openfile !!tfname [ Unix.O_RDWR; Unix.O_CREAT ] 0o600
      with Unix.Unix_error _ ->
        (* alt: just no try and rely on default exn and backtrace *)
        (* alt: call Out.putxxx funcs but mutual recursion *)
        output_string out "?TMP\n";
        (* ed was doing exits(nil) = exit 0 *)
        raise (Exit.ExitCode 0)
       )
    ;
    stdin = Lexing.from_channel (Console.stdin caps);
    out;

    zero = Array.make 10 0;
    dot = 0;
    dol = 0;
    addr1 = 0;
    addr2 = 0;

    savedfile;
    fchange = false;
    count = 0;
    pflag = false;

    vflag = if oflag then false else vflag;
    oflag;
  }

(* TODO? type cmd = Read of range | ... ? *)
