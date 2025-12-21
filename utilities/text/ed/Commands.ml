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
  (* TODO: if not e.given *)
  e.addr1 <- if e.dol > 0 then 1 else 0;
  e.addr2 <- e.dol;
  ()

let squeeze (e : Env.t) (i : int) : unit =
  if e.addr1 < i || e.addr2 > e.dol || e.addr1 > e.addr2
  then begin
      Logs.warn (fun m -> m "can't squeeze");
      Error.e "";
  end

(* was called exfile *)
let exit_file (_e : Env.t) : unit =
  failwith "TODO: exit_file"

(* will return the string with ending \n or None when reached EOF *)
let getfile (e : Env.t) (_chan : Chan.i) () : string option =
  (* alt: do that in caller, again cleaner than in filename *)
  e.count <- e.count + 1;
  failwith "TODO: getfile"

let append (_e : Env.t) _getfile _addr : unit =
  failwith "TODO: append"

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

let read (caps : < Cap.open_in; .. >) (e : Env.t) (file : Fpath.t) : unit =
  try 
    file |> FS.with_open_in caps (fun chan ->
        setwide e;
        squeeze e 0;
        let change = (e.dol != 0) in
        append e (getfile e chan) e.addr2;
        exit_file e;
        e.fchange <- change;
        
    )
  with Sys_error str ->
      Logs.err (fun m -> m "Sys_error: %s" str);
      Error.e !!file


let quit (e : Env.t) : unit =
  if e.vflag && e.fchange && e.dol != 0 then begin
      (* so a second quit will actually quit *)
      e.fchange <- false;
      Logs.warn (fun m -> m "trying to quit with modified buffer");
      Error.e ""
  end;
  (* alt: could also Unix.close e.tfile *)
  Sys.remove !!Env.tfname;
  raise (Exit.ExitCode 0)
