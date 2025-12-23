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

(* store line in tfile and return its offset *)
let putline (e : Env.t) (line : string) : Env.file_offset =
  e.fchange <- true;
  let old_tline = e.tline in
  Unix.lseek e.tfile e.tline Unix.SEEK_SET |> ignore;
  (* alt: could use a different terminator like '\0' in C but simpler to
   * use \n *)
  let line = line ^ "\n" in
  let len = String.length line in
  Unix.write e.tfile (Bytes.of_string line) 0 len |> ignore;
  e.tline <- e.tline + len;
  old_tline

(* dual of putline(), retrieve line in tfile (without trailing '\n') *)
let getline (e : Env.t) (tl : Env.file_offset)  : string =
  Unix.lseek e.tfile tl Unix.SEEK_SET |> ignore;
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


(*****************************************************************************)
(* getfile/putfile (from/to savedfile) *)
(*****************************************************************************)

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
  
(* TODO:
  let string_of_chars xs =
    (* alt: do that in caller, again cleaner than in filename *)
    e.count <- e.count + List.length xs;
    failwith "TODO"
  in
  let rec aux acc
    let copt : char option =
      try Some (input_char chan.i)
      with End_of_file -> None
    in
    (match copt with
    | None -> 
        if acc = []
        then None
        else begin 
           Out.putstr e "\\n appended";
           Some (('\n'::acc) |> List.rev |> string_of_chars)
        end
     | Some c -> aux (c::acc)
    ...
*)

(* dual of getfile() but this time writing all the lines, not just one *)
let putfile (e : Env.t) (chan : Chan.o) : unit =
  for a1 = e.addr1 to e.addr2 do
    let l = getline e e.zero.(a1) ^ "\n" in
    e.count <- e.count + String.length l;
    output_string chan.oc l;
  done
