(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
open Chan

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of cat of Plan 9 *)

type caps = < Cap.open_in; Cap.stdout >

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let cat (_caps : < Cap.stdout; .. >) (chan : Chan.i) : unit =
  Logs.info (fun m -> m "processing %s" (Chan.origin chan));
  failwith "TODO"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)


let main (caps : <caps; ..>) (argv : string array) : Exit.t =
  (* alt: use Arg and process -debug, -verbose, etc. *)
  Logs_.setup (Some Logs.Warning) ();
  (match Array.to_list argv with
  | [] -> raise (Impossible "all programs have at least an argv0")
  | [_argv0] ->
      let chan = { ic = stdin; origin = Chan.Stdin } in
      cat caps chan
  | _argv0::xs ->
      xs |> Fpath_.of_strings |> List.iter (FS.with_open_in caps (cat caps))
  );
  Exit.OK

let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     let argv = CapSys.argv caps in
     Exit.exit caps (Exit.catch (fun () -> main caps argv))
  )
