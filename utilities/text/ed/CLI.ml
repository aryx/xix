(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

open Env
module T = Token

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of ed of Unix and Plan 9.
 *
 * Limitations compared to Plan9 version:
 *  - no unicode (runes) support
 *
 * Improvements over Plan 9 C version:
 *  - far less globals
 *  - no fixed-size array for saved file, buffers, lines, etc.
*)

type caps = < Cap.stdin; Cap.stdout; Cap.stderr; Cap.open_in; Cap.open_out >

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let print_com (_e : Env.t) : unit =
  failwith "TODO: print_com"

let commands caps (e : Env.t) : unit =
  while true do
    if e.pflag then begin
        e.pflag <- false;
        e.addr1 <- e.dot;
        e.addr2 <- e.dot;
        print_com e;
    end;

    let t  = Parse.next_token e in
    (match t with
    | T.Letter c ->
      (match c with
      | 'a' -> failwith "TODO: a"
      | 'r' -> 
          let file = Parse.filename e in
          Commands.read caps e file      
      | c -> failwith (spf "unsupported command '%c'" c)
      );
      (* ed: Error.error "", but because relied on the commands
       * doing some 'continue' which we can't in OCaml so
       * better not use Error.error here by default.
       *)
    | T.EOF ->
       raise (Exit.ExitCode 0)
    | t -> failwith (spf "unexpected token %s" (Token.show t))
    )
  done

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : <caps; ..>) (argv : string array) : Exit.t =
  let args = ref [] in
  (* in "interactive/verbose" mode by default *)
  let vflag = ref true in
  let oflag = ref false in
  let level = ref (Some Logs.Warning) in

  let options = [
     "-", Arg.Clear vflag,
     " non-interactive mode (opposite of verbose)";
    (* 'w' will write to stdout; useful for filters and pipelines *)
     "-o", Arg.Set oflag,
     " write output to standard output instead of modifying the file";

     (* new: *)
     "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose logging mode";
     "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
     " debug logging mode";
  ] |> Arg.align
  in
  (try 
    Arg.parse_argv argv options (fun t -> args := t::!args) 
      (spf "usage: %s [-lwc] [file ...]" argv.(0));
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  Logs_.setup !level ();

  let env : Env.t = Env.init caps !vflag !oflag in
  (* ed: was globp *)
  let first_command = ref None in

  (match !args with
  | [] -> ()
  | [file] -> 
        env.savedfile <- Fpath.v file;
        first_command := Some 'r'
  | _::_::_ -> failwith "too many arguments" 
  );
  if !oflag then first_command := Some 'a';
  Logs.info (fun m -> m "env = %s" (Env.show env));

  while true do
    (* when neither commands() nor quit() raise Error, then
     * quit() will proceed and raise Exit.ExitCode which
     * will exit the loop (and be caught in Main._)
     *)
    try (
        commands caps env;
        Commands.quit env;
    )
    with 
      Error.Error s -> 
        Error.error_1 env ("?" ^ s)
  done;
  Exit.OK
