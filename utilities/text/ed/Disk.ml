(*s: Disk.ml *)
(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to read/write in temporary file (Env.tfile) or specified
 * user file (Env.savedfile).
 *)

(*****************************************************************************)
(* getline/putline (from/to tfile) *)
(*****************************************************************************)
(*s: function [[Disk.putline]] *)
(* store line in tfile and return its offset *)
let putline (e : Env.t) (line : string) : Env.tfile_offset =
  e.fchange <- true;
  let Tfile_offset old_tline = e.tline in
  Unix.lseek e.tfile old_tline Unix.SEEK_SET |> ignore;
  (* alt: could use a different terminator like '\0' in C but simpler to
   * use \n *)
  let line = line ^ "\n" in
  let len = String.length line in
  Unix.write e.tfile (Bytes.of_string line) 0 len |> ignore;
  e.tline <- Tfile_offset (old_tline + len);
  Tfile_offset old_tline
(*e: function [[Disk.putline]] *)
(*s: function [[Disk.getline]] *)
(* dual of putline(), retrieve line in tfile (without trailing '\n') 
 * ed: was taking an Env.tfile_offset but cleaner to take addr
 *)
let getline (e : Env.t) (addr : Env.lineno)  : string =
  let tl = e.zero.(addr).offset in
  let Tfile_offset offset = tl in
  Unix.lseek e.tfile offset Unix.SEEK_SET |> ignore;
  (* alt: Stdlib.input_line (Unix.in_channel_of_descr ...) but then
   * need to close it which unfortunately also close the file_descr so
   * we do our own adhoc input_line below.
   *)
  let bytes = Bytes.of_string " " in
  let rec aux acc =
    let n = Unix.read e.tfile bytes 0 1 in
    let c : char = Bytes.get bytes 0 in
    if n = 1 && c <> '\n'
    then aux (c::acc)
    (* no need to add the \n, putshst will add it *)
    else String_.of_chars (List.rev acc)
  in
  aux []
(*e: function [[Disk.getline]] *)

(*****************************************************************************)
(* getfile/putfile (from/to savedfile) *)
(*****************************************************************************)
(*s: function [[Disk.getfile]] *)
(* will return one line (without trailing '\n') or None when reached EOF *)
let getfile (e : Env.t) (chan : Chan.i) () : string option =
  (* alt: use Stdlib.input_line which does some extra magic around newlines
   * and EOF we want, because ed also uniformize the lack of newline before EOF
   * (see the "\\n appended" message below),but we want to to match exactly what
   * ed does and display the same error message so we need to go lower level
   * than input_line and use input_char directly.
   *)
  try 
    (* 's' will not have the trailing '\n' *)
    let s = input_line chan.ic in
    e.count <- e.count + String.length s + 1 (* to count the new line *);
    Some s
  with End_of_file -> None
(*e: function [[Disk.getfile]] *)
(*s: function [[Disk.putfile]] *)
(* dual of getfile() but this time writing all the lines, not just one *)
let putfile (e : Env.t) (chan : Chan.o) : unit =
  for a1 = e.addr1 to e.addr2 do
    let l = getline e a1 ^ "\n" in
    e.count <- e.count + String.length l;
    output_string chan.oc l;
  done
(*e: function [[Disk.putfile]] *)
(*e: Disk.ml *)
