(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

open Env

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let setwide (e : Env.t) : unit =
  if not e.given then begin
    e.addr1 <- if e.dol > 0 then 1 else 0;
    e.addr2 <- e.dol;
  end;
  ()

let squeeze (e : Env.t) (i : lineno) : unit =
  if e.addr1 < i || e.addr2 > e.dol || e.addr1 > e.addr2
  then begin
      Logs.warn (fun m -> m "can't squeeze");
      Error.e "";
  end

let nonzero (e : Env.t) =
  squeeze e 1

let setnoaddr (e : Env.t) =
  if e.given
  then begin
      Logs.err (fun m -> m "setnoaddr ??");
      Error.e "";
  end

type mode = READ | WRITE

(* ed: "exit" file =~ close file *)
let exfile (e : Env.t) (_m : mode) : unit =
  (* ed: is using passed mode to flush if WRITE but no need in ocaml
   * because closing the channel will flush any remaining IO.
   *)
  if e.vflag then begin
      Out.putd e;
      Out.putchr e '\n';
  end

(*****************************************************************************)
(* getline/putline (from/to tfile) *)
(*****************************************************************************)

(* store line in tfile and return its offset *)
let putline (e : Env.t) (line : string) : file_offset =
  e.fchange <- true;
  let old_tline = e.tline in
  Unix.lseek e.tfile e.tline Unix.SEEK_SET |> ignore;
  let len = String.length line in
  Unix.write e.tfile (Bytes.of_string line) 0 len |> ignore;
  e.tline <- e.tline + len;
  old_tline

(* dual of putline(), retrieve line in tfile (without trailing '\n') *)
let getline (e : Env.t) (tl : file_offset)  : string =
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

(* will return one line (with ending '\n') or None when reached EOF *)
let getfile (e : Env.t) (chan : Chan.i) () : string option =
  (* alt: use Stdlib.input_line which does some extra magic around newlines
   * and EOF we want, because ed also uniformize the lack of newline before EOF
   * (see the "\\n appended" message below),but we want to to match exactly what
   * ed does and display the same error message so we need to go lower level
   * than input_line and use input_char directly.
   *)
  try 
    let s = input_line chan.ic ^ "\n" in
    e.count <- e.count + String.length s;
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

(*****************************************************************************)
(* append in tfile and adjust e.zero *)
(*****************************************************************************)

(* f can be getfile() above or In.gettty() *)
let append (e : Env.t) (f : unit -> string option) (addr : lineno) : int =
  e.dot <- addr;
  let nline = ref 0 in
  let rec aux () =
    match f () with
    | None -> (* EOF *) !nline
    | Some str ->
        if e.dol > Array.length e.zero
        then begin
            failwith "TODO: grow array"
        end;

        let tl = putline e str in
        incr nline;

        e.dol <- e.dol + 1;
        let a1 = ref e.dol in
        let a2 = ref (!a1 + 1) in
        e.dot <- e.dot + 1;
        let rdot = e.dot in
        while !a1 > rdot do
          e.zero.(!a2) <- e.zero.(!a1);
          decr a2; decr a1;
        done;
        e.zero.(rdot) <- tl;
        (* TODO: update zero *)
        aux ()
  in
  aux ()

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Inspecting *)
(* ------------------------------------------------------------------------- *)

(* 'p' *)
let printcom (e : Env.t) : unit =
  nonzero e;
  for a1 = e.addr1 to e.addr2 do
    (* TODO: if listn *)
    Out.putshst e (getline e e.zero.(a1));
  done;
  e.dot <- e.addr2;
  (* TODO: reset flags *)
  ()

(* ------------------------------------------------------------------------- *)
(* Reading *)
(* ------------------------------------------------------------------------- *)

(* 'r' *)
let read (caps : < Cap.open_in; .. >) (e : Env.t) (file : Fpath.t) : unit =
  try 
    file |> FS.with_open_in caps (fun chan ->
        setwide e;
        squeeze e 0;
        let change = (e.dol != 0) in
        append e (getfile e chan) e.addr2 |> ignore;
        exfile e READ;
        e.fchange <- change;
    )
  with Sys_error str ->
    Logs.err (fun m -> m "Sys_error: %s" str);
    Error.e !!file

(* ------------------------------------------------------------------------- *)
(* Writing *)
(* ------------------------------------------------------------------------- *)

(* 'w' *)
let write (caps : < Cap.open_out; ..>) (e : Env.t) (file : Fpath.t) : unit =
  try 
    file |> FS.with_open_out caps (fun chan ->
        (* TODO: when wq (or do in caller in CLI.ml) *)
        setwide e;
        squeeze e (if e.dol > 0 then 1 else 0);

        (* TODO: e.wrapp open without create mode? *)
        e.wrapp <- false;
        if e.dol > 0
        then putfile e chan;

        exfile e WRITE;
        if e.addr1 <= 1 && e.addr2 = e.dol
        then e.fchange <- false;
        (* TODO: when wq *)
    )
  with Sys_error str ->
    Logs.err (fun m -> m "Sys_error: %s" str);
    Error.e !!file

(* ------------------------------------------------------------------------- *)
(* Modifying *)
(* ------------------------------------------------------------------------- *)

(* used for 'a' and 'i' *)
let add (e : Env.t) (i : int) =
  if i <> 0 && (e.given || e.dol > 0) then begin
     e.addr1 <- e.addr1 - 1;
     e.addr2 <- e.addr2 - 1;
  end;
  squeeze e 0;
  In.newline e;
  append e (In.gettty e) e.addr2 |> ignore

(* ------------------------------------------------------------------------- *)
(* Other *)
(* ------------------------------------------------------------------------- *)

(* 'q' *)
let quit (caps : <Cap.open_out; ..>) (e : Env.t) : unit =
  if e.vflag && e.fchange && e.dol != 0 then begin
      (* so a second quit will actually quit *)
      e.fchange <- false;
      Logs.warn (fun m -> m "trying to quit with modified buffer");
      Error.e ""
  end;
  (* alt: could also Unix.close e.tfile *)
  FS.remove caps Env.tfname;
  raise (Exit.ExitCode 0)




