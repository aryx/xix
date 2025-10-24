(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of ar, the Plan 9 archiver.
 *
 * Main limitations compared to ar:
 * 
 * todo?:
 * later:
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)
type caps = < Cap.open_in; Cap.open_out >

let usage = 
  "usage: oar [-options] objects"

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let archive (caps : < Cap.open_in; ..> ) (objfiles : Fpath.t list) (chan : Chan.o) : unit =
  (* TODO? sanity check all of same arch? *)
  let xs = objfiles |> List.map (FS.with_open_in caps Object_file.load) in
  Library_file.save xs chan

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : <caps; ..>) (argv : string array) : Exit.t =
  let infiles = ref [] in
  let outfile = ref (Fpath.v "lib.oa") in

  let level = ref (Some Logs.Warning) in
  (* for debugging *)
  let backtrace = ref false in

  let options = [
    "-o", Arg.String (fun s -> outfile := Fpath.v s),
    spf " <file> output file (default is %s)" !!(!outfile);
    
    (* pad: I added that *)
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " guess what";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";

    (* pad: I added that *)
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";

  ] |> Arg.align
  in
  (try
    Arg.parse_argv argv options
      (fun f -> infiles := Fpath.v f::!infiles) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  Logs_.setup !level ();
  Logs.info (fun m -> m "ar ran from %s" (Sys.getcwd()));

  (match List.rev !infiles with
  | [] -> 
      Arg.usage options usage; 
      Exit.Code 1
  | xs -> 
     try 
        (* the main call *)
        !outfile |> FS.with_open_out caps (fun chan ->
          archive caps xs chan
        );
        Exit.OK
  with exn ->
    if !backtrace
    then raise exn
    else 
      (match exn with
      | Failure s ->
          Logs.err (fun m -> m "%s" s);
          Exit.Code 1
      | _ -> raise exn
      )
  )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     let argv = CapSys.argv caps in
     Exit.exit caps 
        (Exit.catch (fun () -> 
            main caps argv))
  )
