(*s: CLI.ml *)
(* Copyright 2025, 2026 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
module Str = Re_str

open Env
module T = Token
module A = Address

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of ed from Plan 9 (and Unix).
 *
 * Limitations compared to Plan 9 version:
 *  - no unicode (runes) support
 *  - lots of missing features (&, \1, marks, etc.)
 *
 * Improvements over Plan 9 C version:
 *  - support -r restricted mode like in GNU ed
 *  - clearer error messages (via logging)
 *  - far less globals!
 *  - no fixed-size array for saved file, buffers, lines, etc.
 *  - no hard limits on max line size, max size of temp file, 
*)

(*****************************************************************************)
(* Caps *)
(*****************************************************************************)
(*s: type [[CLI.caps]] *)
type caps = < 
    Cap.stdin; Cap.stdout; Cap.stderr;
    Cap.open_in; (* for 'r' *)
    Cap.open_out; (* for 'w' *)
    Cap.forkew; (* for '!' *)
  >
(*e: type [[CLI.caps]] *)
(*s: function [[CLI.restrict_caps]] *)
let restrict_caps rflag (x : < caps; ..>) =
  object
    method exec = 
      if rflag then Error.e_err "!restricted mode on!"
      else x#exec
    method open_in file = 
      if rflag && not (Fpath.is_seg file)
      then Error.e_err (spf "!restricted mode on, can't read %s!" file)
      else x#open_in file
    method open_out file = 
      if rflag && not (Fpath.is_seg file) &&
         (* need open_out to delete tmp file in Commands.quit() *)
         not (file = !!Env.tfname)
      then Error.e_err (spf "!restricted mode on, can't write %s!" file)
      else x#open_out file
    method fork = x#fork
    method wait = x#wait
    method stdin = x#stdin
    method stdout = x#stdout
    method stderr = x#stderr
  end
(*e: function [[CLI.restrict_caps]] *)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)
(*s: function [[CLI.commands]] *)
let rec commands (caps : < Cap.open_in; Cap.open_out; Cap.forkew; ..>)
                 (e : Env.t) : unit =
  (*s: [[CLI.commands()]] debug start *)
  Logs.debug (fun m -> m "commands ->");
  (*e: [[CLI.commands()]] debug start *)
  let done_ = ref false in
  while not !done_ do
    (*s: [[CLI.commands()]], at loop start, if [[pflag]] *)
    if e.pflag then begin
        e.pflag <- false;
        e.addr1 <- e.dot;
        e.addr2 <- e.dot;
        Commands.printcom e;
    end;
    (*e: [[CLI.commands()]], at loop start, if [[pflag]] *)
    let range : Address.range = Address.parse_range e.in_ in
    let (addr1, addr2) = Address.eval_range e range in
    (* TODO: use range.set_dot! *)
    e.addr1 <- addr1;
    e.addr2 <- addr2;
    e.given <- range.given;

    (match Parser.consume e.in_ with
    | T.Char c ->
      (match c with
      (* inspecting *)
      (*s: [[CLI.commands()]] match [[c]] inspecting cases *)
      | 'p' | 'P' ->
         In.newline e;
         Commands.printcom e;
      (*x: [[CLI.commands()]] match [[c]] inspecting cases *)
      | 'f' ->
         (* alt: move in Commands.file() *)
         Commands.setnoaddr e;
         let file : Fpath.t = In.filename e c in
         assert (e.savedfile = Some file);
         Out.putst e !!file;
      (*x: [[CLI.commands()]] match [[c]] inspecting cases *)
      | '=' ->
         (* alt: move in Commands.print_dot_line_number() *)
         Commands.setwide e;
         Commands.squeeze e 0;
         In.newline e;
         e.count <- e.addr2;
         Out.putd e;
         Out.putchr e '\n';
      (*x: [[CLI.commands()]] match [[c]] inspecting cases *)
      (* new: dumping internal state (useful for debugging) *)
      | 'X' -> 
         In.newline e;
         Unix.fsync e.tfile;
         Logs.app (fun m -> m "env = %s\ntfile content =\n%s"
                    (Env.show e)
                    (FS.cat caps Env.tfname |> String.concat "\n"));
      (*e: [[CLI.commands()]] match [[c]] inspecting cases *)
      (* reading *)
      (*s: [[CLI.commands()]] match [[c]] reading case *)
      | 'r' -> 
          let file : Fpath.t = In.filename e c in
          Commands.read caps e file
      (*e: [[CLI.commands()]] match [[c]] reading case *)
      (* writing *)
      (*s: [[CLI.commands()]] match [[c]] writing cases *)
      | 'w' | 'W' ->
         if c = 'W' then e.wrapp <- true;
         (* TODO: if [wW][qQ] *)
         let file : Fpath.t = In.filename e c in
         Commands.write caps e file;
      (*e: [[CLI.commands()]] match [[c]] writing cases *)
      (* modifying *)
      (*s: [[CLI.commands()]] match [[c]] modifying cases *)
      | 'a' -> 
         (*s: [[CLI.commands()]] in [['a']] case, log append mode *)
         Logs.info (fun m -> m "append mode");
         (*e: [[CLI.commands()]] in [['a']] case, log append mode *)
         Commands.add e 0
      (*x: [[CLI.commands()]] match [[c]] modifying cases *)
      | 'i' -> 
         (*s: [[CLI.commands()]] in [['i']] case, log insert mode *)
         Logs.info (fun m -> m "insert mode");
         (*e: [[CLI.commands()]] in [['i']] case, log insert mode *)
         Commands.add e (-1)
      (*x: [[CLI.commands()]] match [[c]] modifying cases *)
      | 'd' ->
         Commands.nonzero e;
         In.newline e;
         Commands.rdelete e e.addr1 e.addr2;
      (*x: [[CLI.commands()]] match [[c]] modifying cases *)
      | 'c' ->
         Commands.nonzero e;
         In.newline e;
         Commands.rdelete e e.addr1 e.addr2;
         Commands.append e (In.gettty e) (e.addr1 - 1) |> ignore;
      (*x: [[CLI.commands()]] match [[c]] modifying cases *)
      | 's' ->
          Commands.nonzero e;
          Commands.substitute e (e.in_.globp <> None);
      (*e: [[CLI.commands()]] match [[c]] modifying cases *)
      (* globals *)
      (*s: [[CLI.commands()]] match [[c]] global cases *)
      | 'g' -> global caps e true
      | 'v' -> global caps e false
      (*e: [[CLI.commands()]] match [[c]] global cases *)
      (* other *)
      (*s: [[CLI.commands()]] match [[c]] other cases *)
      | 'q' | 'Q' ->
         if c = 'Q' then e.fchange <- false;
         Commands.setnoaddr e;
         In.newline e;
         Commands.quit caps e;
      (*x: [[CLI.commands()]] match [[c]] other cases *)
      | '!' -> 
         Commands.callunix caps e
      (*e: [[CLI.commands()]] match [[c]] other cases *)

      | c -> failwith (spf "unsupported command '%c'" c)
      )
      (* ed: was doing error(Q) here but because ed relied on the commands
       * doing some 'continue' which we can't in OCaml so
       * better not use Error.e here
       *)

    | T.Newline ->
        (*s: [[CLI.commands()]] [[Newline]] case *)
        (* print when no command specified, as in 1\n *)

        (* ed: was a1 == nil but simpler to look at given *)
        if not range.given then begin
          (* so any subsequent newline will display a successive line *)
          let a1 = e.dot + 1 in
          e.addr2 <- a1;
          e.addr1 <- a1;
        end;
        (* note that printcom() will internally set e.dot to e.addr2
         * TODO: if lastsep = ';' *)
        Commands.printcom e;
        (*e: [[CLI.commands()]] [[Newline]] case *)

    | T.EOF ->
       (* old: raise (Exit.ExitCode 0) but bad because we need to get to quit()
        * or to return to global() caller when nested call to commands().
        * TODO? raise (Exit.ExitCode 2) instead and check whether in nested
        * commands? so this would force people to use 'q' to quit cleanly
        * (so an error during quit in a script would fail and would
        * make the whole script fail when EOF is reached)
        * or raise EOF and capture it in global() ?
        *)
       done_ := true
    | t -> Parser.was_expecting_but_got "a letter" t
    )
  done;
  (*s: [[CLI.commands()]] debug end *)
  Logs.debug (fun m -> m "commands <-");
  (*e: [[CLI.commands()]] debug end *)
(*e: function [[CLI.commands]] *)
(*s: function [[CLI.global]] *)
(* g/re/cmd, v/re/cmd *)
and global caps (e : Env.t) (pos_or_neg : bool) : unit =

  e.in_.globp |> Option.iter (fun _ ->
      Error.e_err "global command already in"
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
      (* ed: was starting at 0, but then special case later in match()
       * so simpler to start at 1 
       * old: I was recording in a local xs the list of matched lineno, but
       * it does not work because commands() in step2 below may modify 
       * zero which would invalidate the matched lineno of step1
       *)
      for a1 = 1 to e.dol do
        if a1 >= e.addr1 && a1 <= e.addr2 &&
            Commands.match_ e re a1 = pos_or_neg then
          e.zero.(a1).mark <- true
      done;

      (* step2: processing the matching lines
       * old: for a1 = 1 to e.dol, but can't work because
       * commands() below might modify zero and delete lines in which case
       * dol will change dynamicaly.
       *)
      let a1 = ref 1 in
      while !a1 <= e.dol do
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
        incr a1
      done
  
  | t -> Parser.was_expecting_but_got "a regexp" t
(*e: function [[CLI.global]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[CLI.main]] *)
let main (caps : <caps; ..>) (argv : string array) : Exit.t =

  let args = ref [] in
  (*s: [[CLI.main()]] local flags *)
  (* "verbose(interactive)" mode is set by default *)
  let vflag = ref true in
  (*x: [[CLI.main()]] local flags *)
  (* when '-o', the command 'w' will write to stdout (useful for filters) *)
  let oflag = ref false in
  (*x: [[CLI.main()]] local flags *)
  (* new: restricted ed *)
  let rflag = ref false in
  (*x: [[CLI.main()]] local flags *)
  let level = ref (Some Logs.Warning) in
  (*e: [[CLI.main()]] local flags *)

  let options = [
     (*s: [[CLI.main()]] local [[options]] elements *)
     "-", Arg.Clear vflag,
     " non-interactive mode (opposite of verbose)";
     (*x: [[CLI.main()]] local [[options]] elements *)
     "-o", Arg.Set oflag,
     " write buffer to standard output";
     (*x: [[CLI.main()]] local [[options]] elements *)
     "-r", Arg.Set rflag,
     " restricted mode: no shell commands; edits limited to current directory";
     (*x: [[CLI.main()]] local [[options]] elements *)
     (* new: this is verbose *logging*, different from interactive mode *)
     "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose logging mode";
     "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
     " debug logging mode";
     "-quiet", Arg.Unit (fun () -> level := None),
     " quiet logging mode";
     (*e: [[CLI.main()]] local [[options]] elements *)
  ] |> Arg.align
  in
  (* may raise ExitCode *)
  Arg_.parse_argv caps argv options (fun t -> args := t::!args) 
    (spf "usage: %s [options] [file]" argv.(0));
  (*s: [[CLI.main()]] setup logging after parsed argv *)
  Logs_.setup !level ();
  (*e: [[CLI.main()]] setup logging after parsed argv *)
  (*s: [[CLI.main()]] restrict caps *)
  let caps = restrict_caps !rflag caps in
  (*e: [[CLI.main()]] restrict caps *)

  let env : Env.t = Env.init caps !vflag !oflag !rflag in
  (*s: [[CLI.main()]] env adjustments *)
  (match !args with
  | [] -> ()
  | [file] -> 
      env.savedfile <- Some (Fpath.v file);
      env.in_.globp <- Some (Lexing.from_string "r")
  (* stricter: *)
  | _::_::_ -> failwith "too many arguments" 
  );
  (*s: [[CLI.main()]] env adjustments if [[oflag]] *)
  if !oflag then env.in_.globp <- Some (Lexing.from_string "a");
  (*e: [[CLI.main()]] env adjustments if [[oflag]] *)
  (*s: [[CLI.main()]] debug env *)
  Logs.debug (fun m -> m "env = %s" (Env.show env));
  (*e: [[CLI.main()]] debug env *)
  (*e: [[CLI.main()]] env adjustments *)

  while true do
    (* when neither commands() nor quit() raise Error, then
     * quit() will proceed and raise Exit.ExitCode which
     * will exit this loop (and be caught in Main._)
     *)
    try (
        commands caps env;
        Commands.quit caps env;
    )
    with Error.Error s -> 
        (*s: [[CLI.main()]] exn handler for [[Error.Error s]] *)
        (* ed: was in a separate error_1() function *)
        (* TODO: reset globals too? *)
        Out.putchr env '?';
        Out.putst env s;
        (*e: [[CLI.main()]] exn handler for [[Error.Error s]] *)
  done;
  (* should never reach *)
  Exit.OK
(*e: function [[CLI.main]] *)
(*e: CLI.ml *)
