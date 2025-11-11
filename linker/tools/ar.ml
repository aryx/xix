(*s: tools/ar.ml *)
(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of ar, the Plan 9 (object) archiver.
 *
 * Main limitations compared to ar:
 *  - no complex CLI flags; just 'oar objfiles [-o libfile]'
 *    no ar vu, ar rcs, ... just archive!
 *
 * todo:
 *  - index the archive a la SYMDEF/ranlib to reduce size of
 *    binaries in the linker for unneeded object files
 *
 * later:
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)
(*s: type [[Ar.caps]] *)
(* Needs:
 *  - open_in: for argv-derived object files
 *  - open_out: for lib.oa output file
 *)
type caps = < Cap.open_in; Cap.open_out >
(*e: type [[Ar.caps]] *)

(*s: constant [[Ar.usage]] *)
let usage = 
  "usage: oar [-options] objects"
(*e: constant [[Ar.usage]] *)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)
(*s: function [[Ar.archive]] *)
let archive (caps : < Cap.open_in; ..> ) (objfiles : Fpath.t list) (chan : Chan.o) : unit =
  (* sanity checks *)
  (* TODO? sanity check all of same arch? *)
  objfiles |> List.iter (fun file ->
    if not (Object_file.is_obj_filename file)
    then failwith (spf "The file extension of %s does not match an object file"
          !!file)
  );
  let libfile = Fpath.v (Chan.destination chan) in
  if not (Library_file.is_lib_filename libfile)
  then failwith (spf "The file extension of %s does not match a library file"
          !!libfile);

  let xs = objfiles |> List.map (FS.with_open_in caps Object_file.load) in
  Library_file.save xs chan
(*e: function [[Ar.archive]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[Ar.main]] *)
let main (caps : <caps; ..>) (argv : string array) : Exit.t =
  let infiles = ref [] in
  let outfile = ref (Fpath.v "lib.oa") in

  let level = ref (Some Logs.Warning) in
  (* for debugging *)
  let backtrace = ref false in

  let options = ([
    "-o", Arg.String (fun s -> outfile := Fpath.v s),
    spf " <file> output file (default is %s)" !!(!outfile);
    
    (* pad: I added that *)
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";

  ] @ Logs_.cli_flags level) 
   |> Arg.align
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
(*e: function [[Ar.main]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: constant [[Ar._]] *)
let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     let argv = CapSys.argv caps in
     Exit.exit caps (Exit.catch (fun () -> main caps argv))
  )
(*e: constant [[Ar._]] *)
(*e: tools/ar.ml *)
