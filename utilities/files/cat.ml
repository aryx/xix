(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of cat of Plan 9.
 *
 * Improvements over Plan 9 C version:
 *  - no need sysfatal() calls and error management as default Unix exn
 *    (e.g., file not found, write error, error reading) and its pretty printing
 *    should be good enough
 *  - no need open/close, use higher-order FS.with_open_in
 *
 * Still the OCaml version is longer than the C one.
*)

type caps = < Cap.open_in; Cap.stdout >

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let cat (caps : < Cap.stdout; .. >) (chan : Chan.i) : unit =
  Logs.info (fun m -> m "processing %s" (Chan.origin chan));

  let buf = Bytes.create 8192 in
  let rec aux () =
    let n = input chan.ic buf 0 8192 in
    if n = 0
    then ()
    else begin
        output (Console.stdout caps) buf 0 n;
        aux ()
      end
  in
  aux ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : <caps; ..>) (argv : string array) : Exit.t =
  (* alt: use Arg and process -debug, -verbose, etc. *)
  Logs_.setup (Some Logs.Warning) ();
  (match Array.to_list argv with
  | [] -> raise (Impossible "all programs have at least an argv0")
  | [_argv0] ->
      let chan = Chan.{ ic = stdin; origin = Chan.Stdin } in
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
