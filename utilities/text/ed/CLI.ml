(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

open Env
module T = Token
module A = Address

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of ed of Unix and Plan 9.
 *
 * Limitations compared to Plan9 version:
 *  - no unicode (runes) support
 *
 * Improvements over Plan 9 C version:
 *  - far less globals!
 *  - no fixed-size array for saved file, buffers, lines, etc.
 *  - no hard limits on max line size, max size of temp file, 
 *  - clearer error messages (via logging)
*)

type caps = < 
    Cap.stdin; Cap.stdout; Cap.stderr;
    Cap.open_in; Cap.open_out;
  >

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* this will be called from CLI.main() in an handler for the Error exn *)
let error_1 (e : Env.t) (s : string) : unit =
  (* TODO: reset globals too? *)
  Out.putchr e '?';
  Out.putst e s;
  ()

let rec eval_address (e : Env.t) (a : Address.t) : Env.lineno =
  match a with
  | A.Current -> e.dot
  | A.Last -> e.dol
  | A.Line n -> n
  | A.Mark _ -> failwith "TODO: Mark"
  | A.SearchFwd _ | A.SearchBwd _ -> failwith "TODO: SearchXxx"
  | A.Relative (x, n) -> eval_address e x + n

let eval_range (e : Env.t) (r : Address.range) : Env.lineno * Env.lineno =
  Logs.debug (fun m -> m "range = %s" (Address.show_range r));

  let addr2 = eval_address e r.addr2 in
  let addr1 =
    match r.addr1 with
    | None -> addr2
    | Some a -> eval_address e a
  in
  addr1, addr2

let match_ (e : Env.t) (re : Env.regex) (addr : lineno) : bool =
  let line = Disk.getline e e.zero.(addr).offset in
  Str.string_match re line 0

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let rec commands (caps : < Cap.open_in; Cap.open_out; ..>) (e : Env.t) : unit =
  Logs.debug (fun m -> m "commands ->");
  let done_ = ref false in

  while not !done_ do

    if e.pflag then begin
        e.pflag <- false;
        e.addr1 <- e.dot;
        e.addr2 <- e.dot;
        Commands.printcom e;
    end;

    let range : Address.range = Parser.parse_address_range e.in_  in
    let (addr1, addr2) = eval_range e range in
    e.addr1 <- addr1;
    e.addr2 <- addr2;
    e.given <- range.given;

    let t  = Parser.consume e.in_ in
    (match t with

    | T.Char c ->
      (match c with
      (* inspecting *)

      | 'p' | 'P' ->
         In.newline e;
         Commands.printcom e;
      | 'f' ->
         (* alt: move in Commands.file() *)
         Commands.setnoaddr e;
         let file : Fpath.t = In.filename e c in
         assert (e.savedfile = Some file);
         Out.putst e !!file;
      | '=' ->
         (* alt: move in Commands.print_dot_line_number() *)
         Commands.setwide e;
         Commands.squeeze e 0;
         In.newline e;
         e.count <- e.addr2;
         Out.putd e;
         Out.putchr e '\n';
      (* new: *)
      | 'X' -> 
         In.newline e;
         Unix.fsync e.tfile;
         Logs.app (fun m -> m "env = %s\ntfile content =\n%s"
                    (Env.show e)
                    (FS.cat caps Env.tfname |> String.concat "\n"));

      (* reading *)

      | 'r' -> 
          let file : Fpath.t = In.filename e c in
          Commands.read caps e file      

      (* writing *)
      | 'w' | 'W' ->
         if c = 'W' then e.wrapp <- true;
         (* TODO: if [wW][qQ] *)
         let file : Fpath.t = In.filename e c in
         Commands.write caps e file;

      (* modifying *)

      | 'a' -> 
         Logs.info (fun m -> m "append mode");
         Commands.add e 0
      | 'i' -> 
         Logs.info (fun m -> m "insert mode");
         Commands.add e (-1)
      | 'd' ->
         Commands.nonzero e;
         In.newline e;
         Commands.rdelete e e.addr1 e.addr2;
      | 'c' ->
         Commands.nonzero e;
         In.newline e;
         Commands.rdelete e e.addr1 e.addr2;
         Commands.append e (In.gettty e) (e.addr1 - 1) |> ignore;

      (* globals *)
      | 'g' -> global caps e true
      | 'v' -> global caps e false

      (* other *)
      | 'q' | 'Q' ->
         if c = 'Q' then e.fchange <- false;
         Commands.setnoaddr e;
         In.newline e;
         Commands.quit caps e;

      | c -> failwith (spf "unsupported command '%c'" c)
      );
      (* ed: Error.error "", but because relied on the commands
       * doing some 'continue' which we can't in OCaml so
       * better not use Error.error here by default.
       *)

    (* TODO: even regular ed seems not do work for newline *)
    | T.Newline ->
        (* print when no command specified, as in 1\n *)
        (* TODO: if a1 = None *)
        let a1 = e.dot + 1 in
        e.addr2 <- a1;
        e.addr1 <- a1;
        
        (* TODO: if lastsep = ';' *)
        Commands.printcom e;

    | T.EOF ->
       (* not raise (Exit.ExitCode 0) because we need to get to quit()
        * or to return to global() caller when nested call to commands().
        *)
       done_ := true
    | t -> Parser.was_expecting_but_got "a letter" t
    )
  done;
  Logs.debug (fun m -> m "commands <-");

(* g/re/cmd, v/re/cmd *)
and global caps (e : Env.t) (pos_or_neg : bool) : unit =

  e.in_.globp |> Option.iter (fun _ ->
      Logs.err (fun m -> m "global command already in");
      Error.e ""
  );
  Commands.setwide e;
  Commands.squeeze e (if e.dol > 0 then 1 else 0);

  match Parser.consume e.in_ with
  | Slash str ->
      let re = Str.regexp str in
      (* TODO: Lexer.line_or_escaped_lines *)
      let line = Lexer.line e.in_.stdin in
      let line = if line = "" then "p" else line in

      (* step1: marking the matching lines *)
      (* ed: was starting at 0, but then special casing it later in match()
       * so simpler to start at 1 
       * old: I was recording in a local xs the list of matched lineno, but
       * it does not work because commands() in step2 below may modify 
       * zero which would invalidate the matched lineno of step1
       *)
      for a1 = 1 to e.dol do
        if a1 >= e.addr1 && a1 <= e.addr2 && match_ e re a1 = pos_or_neg then
          e.zero.(a1).mark <- true
      done;

      (* step2: processing the matching lines
       * old: for a1 = 1 to e.dol, but can't work because
       * commands() below might modify zero and reduce it in which case
       * dol will change dynamicaly.
       *)
      let rec aux a =
        let a1 = ref a in
        if !a1 <= e.dol then begin
          if e.zero.(!a1).mark then begin
            e.zero.(!a1).mark <- false;
            e.dot <- !a1;
            (* ugly: need trailing \n otherwise T.EOF would be consumed
             * too early by In.Newline().
             *)
            e.in_.globp <- Some (Lexing.from_string (line ^ "\n"));
            (* recurse!! *)
            commands caps e;
            (* need to restart from scratch, but with one less marked line *)
            a1 := 0
          end;
          aux (!a1 + 1)
        end
      in
      aux 1
  
  | t -> Parser.was_expecting_but_got "a regexp" t

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : <caps; ..>) (argv : string array) : Exit.t =
  let args = ref [] in
  (* "verbose(interactive)" mode is set by default *)
  let vflag = ref true in
  let oflag = ref false in
  let level = ref (Some Logs.Warning) in

  let options = [
     "-", Arg.Clear vflag,
     " non-interactive mode (opposite of verbose)";
    (* when '-o', command 'w' will write to stdout (useful for filters) *)
     "-o", Arg.Set oflag,
     " write output to standard output instead of modifying the file";

     (* new: this is verbose *logging*, different from interactive mode *)
     "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose logging mode";
     "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
     " debug logging mode";
     "-quiet", Arg.Unit (fun () -> level := None),
     " quite logging mode";
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

  (match !args with
  | [] -> ()
  | [file] -> 
      env.savedfile <- Some (Fpath.v file);
      env.in_.globp <- Some (Lexing.from_string "r")
  | _::_::_ -> 
      (* stricter: *)
      failwith "too many arguments" 
  );
  if !oflag then env.in_.globp <- Some (Lexing.from_string "a");
  Logs.debug (fun m -> m "env = %s" (Env.show env));

  while true do
    (* when neither commands() nor quit() raise Error, then
     * quit() will proceed and raise Exit.ExitCode which
     * will exit this loop (and be caught in Main._)
     *)
    try (
        commands caps env;
        Commands.quit caps env;
    )
    with Error.Error s -> error_1 env ("?" ^ s)
  done;
  Exit.OK
