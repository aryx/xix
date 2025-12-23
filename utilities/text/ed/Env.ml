(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A set of globals used by many functions.
 *
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* LATER: use Tmp.with_new_file *)
let tfname = Fpath.v "/tmp/oed.scratch"

(* offset in tfname file content *)
type file_offset = int
[@@deriving show]

type offset_and_mark = {
  (* offset in tfile *)
  offset: file_offset;
  (* used by the 'g' or 'v' commands to mark matched lines *)
  mutable mark: bool;
}
[@@deriving show]
(* alt: use None *)
let no_line = { offset = 0; mark = false }

(* ed uses 1-indexed line numbers, but 0 is also used as a special value.
 * alt: call it cursor?
*)
type lineno = int
[@@deriving show]

(* alt: Re.t *)
type regex = Str.regexp

(* The globals *)
type t = {
  (* to read the user commands from (and also line input in 'a'/'i' modes) *)
  in_: Parser.state;
  (* stdout unless oflag is set in which case it's stderr *)
  out: Out_channel_.t;

  (* This is the temporary tfname file, ed backing store!
   * Note that we can't use the OCaml usual {in/out}_channel type because we
   * need to both read and write in the temporary file, hence the use of the
   * more general Unix.file_descr.
   *)
  tfile : Unix_.file_descr;
  (* current write file offset in tfile to append new lines *)
  mutable tline : file_offset;

  (* growing array of line offsets in tfile. 1-indexed array but the 0
   * entry is used as a sentinel. map lineno -> file_offset.
   * The bool is a mark used to remember a matching line in g/re/x
   * operations.
   *)
  mutable zero : offset_and_mark array;
  (* index entried in zero *)
  (* current line *)
  mutable dot: lineno;
  (* last line (dollar) *)
  mutable dol: lineno;

  (* for 1,3p commands. See also Address.range, but here we have
   * concrete line number, not symbolic "addresses".
   *)
  mutable addr1: lineno;
  mutable addr2: lineno;
  mutable given: bool;

  (* for 'w', 'r', 'f' *)
  mutable savedfile: Fpath.t option;
  (* write append, for 'W' *)
  mutable wrapp : bool;
  (* did the buffer changed (mostly tested with dol > 0) *)
  mutable fchange: bool;

  (* count #chars read, or number of lines; displayed by Out.putd() *)
  mutable count: int;
  (* set by ?? effect is to Out.printcom() in commands () before the next cmd *)
  mutable pflag: bool;
  (* ?? what functions rely on column number set? *)
  mutable col: int;

  (* verbose (a.k.a. interactive) flag, cleared by 'ed -' *)
  vflag: bool;
  (* output flag, set by 'ed -o'.
   * used just in Out.putchr() so could be removed almost.
   *)
  oflag: bool;
}
[@@deriving show]

(*****************************************************************************)
(* init() *)
(*****************************************************************************)

let init (caps : < Cap.stdin; Cap.stdout; Cap.stderr; ..>) 
     (vflag : bool) (oflag : bool) : t =
  let out = if oflag then Console.stderr caps else Console.stdout caps in
  (* will be overwritten possibly in the caller by argv[1] 
   * TODO: works on Linux? /fd/1 exists?
   *)
  let savedfile = if oflag then Some (Fpath.v "/fd/1") else None in
  { 
    in_ = Parser.init caps;
    out;

    tfile =
      (try
        Unix.openfile !!tfname [ Unix.O_RDWR; Unix.O_CREAT ] 0o600
      with Unix.Unix_error (err, s1, s2) ->
        Logs.err (fun m -> m "%s %s %s" (Unix.error_message err) s1 s2);
        (* alt: just no try and rely on default exn and backtrace *)
        (* alt: call Out.putxxx funcs but mutual recursion *)
        output_string out "?TMP\n";
        (* ed was doing exits(nil) = exit 0 so we do the same *)
        raise (Exit.ExitCode 0)
       );
    (* sentinel value so that file offsets 0 and 1 are reserved and no
     * real line offsets in zero[] can have those values
     *)
    tline = 2;

    zero = Array.make 10 no_line;
    dot = 0;
    dol = 0;
    addr1 = 0;
    addr2 = 0;
    given = false;

    savedfile;
    fchange = false;
    wrapp = false;
    count = 0;
    pflag = false;
    col = 0;

    vflag = if oflag then false else vflag;
    oflag;
  }
