(* Copyright 2026 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Toy hello program acting as a template for new projects in xix hello.
 *
 * Limitations compared to Plan 9 version:
 *  - no unicode (runes) support
 *  - lots of missing features
 *
 * Improvements over Plan 9 C version:
 *  - clearer error messages (via logging)
 *  - far less globals!
*)

(*****************************************************************************)
(* Caps *)
(*****************************************************************************)

type caps = < Cap.stdout; Cap.stdin; Cap.open_in >

type conf = {
  iflag : bool;
  nflag: bool;
}

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let grep (caps : < Cap.stdout; ..>) (conf : conf) (re_str : string)
     (chan : Chan.i) : unit =
  Logs.info (fun m -> m "processing: %s with regexp |%s|" 
        (Chan.origin chan) re_str);

  let re = Re_perl.compile_pat
      (if conf.iflag then [ Re_perl.Caseless ] else [])
      re_str 
  in

  let line = ref 0 in
  let print_match str = 
    Console.print caps (spf "%s%s" 
      (if conf.nflag then spf "%d:" !line else "") str);
  in
  let rec loop () =
    incr line;
    let str_opt = 
      try Some (input_line chan.ic)
      with End_of_file -> None
    in
    match str_opt with
    | None -> ()
    | Some str ->
        if Re_core.execp 0 (-1) re str
        then print_match str;
        loop ()
  in
  loop ()
    
  

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : <caps; Cap.stderr; ..>) (argv : string array) : Exit.t =

  let args = ref [] in
  let iflag = ref false in
  let nflag = ref false in
  let level = ref (Some Logs.Warning) in

  let usage = 
    spf "usage: %s [options] [file]" argv.(0)
  in
  let options = ([
     "-i", Arg.Set iflag,
     " ignore case distinctions in patterns and data";
     "-n", Arg.Set nflag,
     " print line number with output lines";
    ] @ Logs_.cli_flags level
   ) |> Arg.align
  in
  (* may raise ExitCode *)
  Arg_.parse_argv caps argv options (fun t -> args := t::!args) usage;
  Logs_.setup !level ();

  let conf = { nflag = !nflag; iflag = !iflag } in

  (match List.rev !args with
  | [] -> 
        Arg.usage options usage; 
        raise (Exit.ExitCode 2)
  | [re_str] -> 
      let chan = { Chan.ic = Console.stdin caps; origin = Chan.Stdin } in
      grep caps conf re_str chan
                   
  | re_str::xs -> 
     xs |> List.iter (fun file ->
        Fpath.v file |> FS.with_open_in caps (fun chan ->
           grep caps conf re_str chan
        )
    )
  );
  Exit.OK
