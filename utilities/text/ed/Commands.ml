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

let setwide (_e : Env.t) : unit =
  failwith "TODO: setwide"

let squeeze (_e : Env.t) (_n : int) : unit =
  failwith "TODO: squeeze"

let append (_e : Env.t) _getfile _addr2 : unit =
  failwith "TODO: append"

(* was called exfile *)
let exit_file (_e : Env.t) : unit =
  failwith "TODO: exit_file"

let getfile () =
  failwith "TODO: getfile"

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

let read (caps : < Cap.open_in; .. >) (e : Env.t) (file : Fpath.t) : unit =
  try 
    file |> FS.with_open_in caps (fun _chan ->
        setwide e;
        squeeze e 0;
        let change = (e.dol != 0) in
        append e getfile e.addr2;
        exit_file e;
        e.fchange <- change;
        
    )
  with Sys_error _str ->
      Error.e !!file


let quit (e : Env.t) : unit =
  if e.vflag && e.fchange && e.dol != 0 then begin
      (* so a second quit will actually quit *)
      e.fchange <- false;
      Error.e ""
  end;
  (* alt: could also Unix.close e.tfile *)
  Sys.remove !!Env.tfname;
  raise (Exit.ExitCode 0)
