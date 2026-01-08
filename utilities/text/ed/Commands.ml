(*s: Commands.ml *)
(* Copyright 2025, 2026 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

open Env

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The main commands *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Commands.setwide]] *)
let setwide (e : Env.t) : unit =
  if not e.given then begin
    e.addr1 <- if e.dol > 0 then 1 else 0;
    e.addr2 <- e.dol;
  end;
  ()
(*e: function [[Commands.setwide]] *)
(*s: function [[Commands.squeeze]] *)
let squeeze (e : Env.t) (i : lineno) : unit =
  if e.addr1 < i || e.addr2 > e.dol || e.addr1 > e.addr2 
  then Error.e_warn "can't squeeze"
(*e: function [[Commands.squeeze]] *)
(*s: function [[Commands.nonzero]] *)
let nonzero (e : Env.t) =
  squeeze e 1
(*e: function [[Commands.nonzero]] *)
(*s: function [[Commands.setnoaddr]] *)
let setnoaddr (e : Env.t) =
  if e.given
  then Error.e_err "setnoaddr ??"
(*e: function [[Commands.setnoaddr]] *)

(*s: type [[Commands.mode]] *)
type mode = READ | WRITE
(*e: type [[Commands.mode]] *)
(*s: function [[Commands.exfile]] *)
(* ed: "exit" file =~ close file *)
let exfile (e : Env.t) (_m : mode) : unit =
  (* ed: is using passed mode to flush if WRITE but no need in ocaml
   * because closing the channel will flush any remaining IO.
   *)
  if e.vflag then begin
      Out.putd e;
      Out.putchr e '\n';
  end
(*e: function [[Commands.exfile]] *)

let match_ (e : Env.t) (re : Regex.t) (addr : Env.lineno) : bool =
  let line = Disk.getline e addr in
  Regex.match_str re line

let file_in_current_dir (file : Fpath.t) : bool =
  (* alt: use Unix.realpath, follow symlink and compare to cwd *)
  Fpath.is_seg !!file

(*****************************************************************************)
(* append in tfile and adjust e.zero *)
(*****************************************************************************)
(*s: function [[Commands.append]] *)
(* f can be Disk.getfile() or In.gettty() *)
let append (e : Env.t) (f : unit -> string option) (addr : lineno) : int =
  e.dot <- addr;
  let nline = ref 0 in

  let rec aux () =
    match f () with
    | None -> (* EOF *) !nline
    | Some str ->
        if e.dol + 2 >= Array.length e.zero 
        then begin
            let oldz = e.zero in
            let len = Array.length oldz in
            let newz  = Array.make (len + 512) Env.no_line in
            Array.blit oldz 0 newz 0 len;
            e.zero <- newz;
        end;

        let tl = Disk.putline e str in
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
        e.zero.(rdot) <- {offset = tl; mark = false};
        (* TODO: update zero *)
        aux ()
  in
  aux ()
(*e: function [[Commands.append]] *)

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Inspecting *)
(* ------------------------------------------------------------------------- *)
(*s: function [[Commands.printcom]] *)
(* 'p' *)
let printcom (e : Env.t) : unit =
  nonzero e;
  for a1 = e.addr1 to e.addr2 do
    (* TODO: if listn *)
    Out.putshst e (Disk.getline e a1);
  done;
  e.dot <- e.addr2;
  (* TODO: reset flags *)
  ()
(*e: function [[Commands.printcom]] *)

(* ------------------------------------------------------------------------- *)
(* Reading *)
(* ------------------------------------------------------------------------- *)
(*s: function [[Commands.read]] *)
(* 'r' *)
let read (caps : < Cap.open_in; .. >) (e : Env.t) (file : Fpath.t) : unit =
  (* ocaml-light: see callunix() comment below *)
  if e.rflag && not (file_in_current_dir file)
  then Error.e_err (spf "restricted mode on, can't access %s" !!file);
  try 
    file |> FS.with_open_in caps (fun chan ->
        setwide e;
        squeeze e 0;
        let change = (e.dol != 0) in
        append e (Disk.getfile e chan) e.addr2 |> ignore;
        exfile e READ;
        e.fchange <- change;
    )
  with Sys_error str ->
    Logs.err (fun m -> m "Sys_error: %s" str);
    Error.e_legacy !!file
(*e: function [[Commands.read]] *)

(* ------------------------------------------------------------------------- *)
(* Writing *)
(* ------------------------------------------------------------------------- *)
(*s: function [[Commands.write]] *)
(* 'w' *)
let write (caps : < Cap.open_out; ..>) (e : Env.t) (file : Fpath.t) : unit =
  (* ocaml-light: see callunix() comment below *)
  if e.rflag && not (file_in_current_dir file)
  then Error.e_err (spf "restricted mode on, can't access %s" !!file);
  try 
    file |> FS.with_open_out caps (fun chan ->
        (* TODO: when wq (or do in caller in CLI.ml) *)
        setwide e;
        squeeze e (if e.dol > 0 then 1 else 0);

        (* TODO: e.wrapp open without create mode? *)
        e.wrapp <- false;
        if e.dol > 0
        then Disk.putfile e chan;

        exfile e WRITE;
        if e.addr1 <= 1 && e.addr2 = e.dol
        then e.fchange <- false;
        (* TODO: when wq *)
    )
  with Sys_error str ->
    Logs.err (fun m -> m "Sys_error: %s" str);
    Error.e_legacy !!file
(*e: function [[Commands.write]] *)

(* ------------------------------------------------------------------------- *)
(* Modifying *)
(* ------------------------------------------------------------------------- *)
(*s: function [[Commands.add]] *)
(* used for 'a' and 'i' *)
let add (e : Env.t) (i : int) =
  if i <> 0 && (e.given || e.dol > 0) then begin
     e.addr1 <- e.addr1 - 1;
     e.addr2 <- e.addr2 - 1;
  end;
  squeeze e 0;
  In.newline e;
  append e (In.gettty e) e.addr2 |> ignore
(*e: function [[Commands.add]] *)

(*s: function [[Commands.rdelete]] *)
(* used for 'r' and 'c' *)
let rdelete (e : Env.t) (ad1 : lineno) (ad2 : lineno) =
  let a1 = ref ad1 in
  let a2 = ref (ad2 + 1) in
  let a3 = e.dol in
  e.dol <- e.dol - (!a2 - !a1);
  let rec aux () =
    e.zero.(!a1) <- e.zero.(!a2);
    incr a1;
    incr a2;
    if !a2 <= a3
    then aux ()
  in
  aux ();
  a1 := ad1;
  if !a1 > e.dol then a1 := e.dol;
  e.dot <- !a1;
  e.fchange <- true
(*e: function [[Commands.rdelete]] *)

(* used for 's' *)
let substitute (e : Env.t) (_inglob: bool) : unit =
  (* TODO: handle inglob *)
  match Parser.consume e.in_ with
  (* handle | Question re_str? does not really make sense. *)
  | Slash re_str ->
      let re = Str.regexp re_str in
      (* ugly: *) 
      Buffer.clear Lexer.buf;
      let subst_str = Lexer.regexp '/' e.in_.stdin in
      (* TODO: parse 's/.../.../g' *)
      In.newline e;

      for a1 = e.addr1 to e.addr2 do
        let line = Disk.getline e a1 in
        (* TODO: handle 's/.../.../g' *)
        if Regex.match_str re line
        then begin
           let loc1 = Str.match_beginning () in
           let loc2 = Str.match_end () in
           let len = String.length line in
           
           let before = String.sub line 0 loc1 in
           let _matched = String.sub line loc1 (loc2 - loc1) in
           let after = String.sub line loc2 (len - loc2) in
           (* TODO: handle & and \1 in subst_str *)
           let newline = before ^ subst_str ^ after in
           let tl = Disk.putline e newline in
           (* TODO: mark? *)
           e.zero.(a1) <- { offset = tl; mark = false }
        end
      done

  | t -> Parser.was_expecting_but_got "a regexp" t

(* ------------------------------------------------------------------------- *)
(* Other *)
(* ------------------------------------------------------------------------- *)
(*s: function [[Commands.quit]] *)
(* 'q' *)
let quit (caps : <Cap.open_out; ..>) (e : Env.t) : unit =
  if e.vflag && e.fchange && e.dol != 0 then begin
      (* so a second quit will actually quit *)
      e.fchange <- false;
      Error.e_warn "trying to quit with modified buffer"
  end;
  (* alt: could also Unix.close e.tfile *)
  FS.remove caps Env.tfname;
  raise (Exit.ExitCode 0)
(*e: function [[Commands.quit]] *)

(* '! *)
let callunix (caps : <Cap.forkew; ..>) (e: Env.t) : unit =
  setnoaddr e;
  let s = In.gety e in
  (* needed only for ocaml-light who does not support method call
   * and so can't provide the dynamic caps of CLI.restrict_caps
   *)
  if e.rflag 
  then Error.e_err "restricted mode on";
  (* ed: was calling rc -c but here we reuse (Cap)Sys.command which relies
   * on the default shell
   *)
  let _ret = CapSys.command caps s in
  if e.vflag
  then Out.putst e "!"
(*e: Commands.ml *)
