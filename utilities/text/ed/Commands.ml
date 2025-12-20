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

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

let quit (e : Env.t) =
  if e.vflag && e.fchange && e.dol != 0 then begin
      (* so a second quit will actually quit *)
      e.fchange <- false;
      Error.error ""
  end;
  (* alt: could also Unix.close e.tfile *)
  Sys.remove !!Env.tfname;
  raise (Exit.ExitCode 0)
