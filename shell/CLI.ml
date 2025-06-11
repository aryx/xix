(*s: shell/CLI.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)

open Common
open Fpath_.Operators

module R = Runtime
module O = Opcode

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of rc, the Plan 9 shell.
 *
 * Main limitations compared to rc:
 *  - no unicode support
 *  - not all of the fancy redirections and fancy pipes
 *  - no storing of functions in the environment
 *    (used by rcmain. But can do the same by using '. rcmain')
 * 
 * Improvements (IMHO):
 *  - a strict mode where we report when deleting undefined function
 *  - Logs with errors and warnings and debug
 *  - more?
 * 
 * todo:
 *  - read environment variables and export variables
 *  - globbing
 *  - Isatty rc -i detection
 *  - add ~ shortcut for HOME (from csh?)
 *  - rc -c
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(*s: type [[CLI.caps]] *)
(* Need:
 *  - fork/exec: obviously as we are a shell
 *  - chdir: for the builtin 'cd'
 *  - env: to ??
 *  - exit: as many commands can abruptely exit 'rc' itself or children
 *    created by 'rc'
 *  - open_in: ??
 *
 * alt: could remove Cap.exit and use Exit.ExitCode exn in Process.ml instead
*)
type caps = < Cap.fork; Cap.exec; Cap.chdir; Cap.env; Cap.exit; Cap.open_in >
(*e: type [[CLI.caps]] *)

(* -d and -p are dead according to man page so I removed them *)
(*s: constant [[CLI.usage]] *)
let usage =
  "usage: rc [-SsriIlxevV] [-c arg] [-m command] [file [arg ...]]"
(*e: constant [[CLI.usage]] *)

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

(*s: function [[CLI.do_action]] *)
let do_action caps s xs =
  match s with
  | "-test_parser" ->
      xs |> List.iter (fun file ->
        let file = Fpath.v file in
        Logs.info (fun m -> m "processing %s" !!file);
        file |> FS.with_open_in caps (fun (chan : Chan.i) ->
          let lexbuf = Lexing.from_channel chan.Chan.ic in

          (* for error reporting I need a runq *)
          let t = Runtime.mk_thread [||] 0 (Hashtbl.create 0) in
          R.runq := t::!R.runq;
          t.R.file <- Some !!file;

          let rec loop () =
            let line = Parse.parse_line lexbuf in
            match line with
            | Some seq -> 
              Logs.app (fun m -> m "%s" (Dumper_.s_of_cmd_sequence seq));
              loop ();
            | None -> ()
          in
          loop ()
       )
      )

  | _ -> failwith ("action not supported: " ^ s)
  (* old: do that in caller now, so more explicit 
  runq := t::!runq
  *)
(*e: function [[CLI.do_action]] *)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

(*s: constant [[CLI._bootstrap_simple]] *)
let _bootstrap_simple : O.opcode array = 
  [| O.F O.REPL |]
(*e: constant [[CLI._bootstrap_simple]] *)
(*s: function [[CLI.bootstrap]] *)
(* The real one is more complex:
 *  *=(argv);. /usr/lib/rcmain $*
 * Boostrap is now a function because it uses a flag that can be
 * modified after startup.
 *)
let bootstrap () : O.opcode array = 
  [| 
      O.F O.Mark;
        O.F O.Word;
        O.S "*";
      O.F O.Assign; (* will pop_list twice *)

      O.F O.Mark;
        O.F O.Mark;
          O.F O.Word;
          O.S "*";
        O.F O.Dollar; (* will pop_list once *)
        O.F O.Word;
        O.S !Flags.rcmain; (* can be changed -m *)
        O.F O.Word;
        O.S ".";
      O.F O.Simple; (* will pop_list once *)

      O.F O.Exit;
  |]
(*e: function [[CLI.bootstrap]] *)

(*s: function [[CLI.interpret_bootstrap]] *)
let interpret_bootstrap (caps : < caps >) (args : string list) : unit =
  let t = R.mk_thread (bootstrap ()) 0 (Hashtbl.create 11) in
  R.runq := t::!R.runq;

  (*s: [[CLI.interpret_bootstrap()]] other initializations for [[t]] *)
  (* less: set argv0 *)
  args |> List.rev |> List.iter Runtime.push_word;
  (*x: [[CLI.interpret_bootstrap()]] other initializations for [[t]] *)
  t.R.lexbuf <- Lexing.from_channel stdin;
  (*x: [[CLI.interpret_bootstrap()]] other initializations for [[t]] *)
  t.R.iflag <- !Flags.interactive;
  (*e: [[CLI.interpret_bootstrap()]] other initializations for [[t]] *)

  while true do
    (* bugfix: need to fetch the current thread each time,
     * as the interpreted code may have modified runq.
     *)
    let t = Runtime.cur () in
    let pc = t.R.pc in
    (*s: [[CLI.interpret()]] if [[rflag]] *)
    (* less: cycle =~ codevec pointer *)
    if !Flags.rflag
    then Logs.app (fun m -> m "pid %d %d %s %s"
                (Unix.getpid ())
                !pc
                (Dumper_.s_of_opcode t.R.code.(!pc))
                ( (t.R.argv::t.R.argv_stack) |> List.map (fun xs ->
                    spf "(%s)" (String.concat " " xs)
                   ) |> String.concat " "));
    (*e: [[CLI.interpret()]] if [[rflag]] *)
    incr pc;
    (match t.R.code.(!pc - 1) with
    (* opcode dispatch ! *)
    | O.F operation ->  Interpreter.interpret_operation caps operation
    | O.S s -> failwith (spf "was expecting a F, not a S: %s" s)
    | O.I i -> failwith (spf "was expecting a F, not a I: %d" i)
    );
    (* todo: handle trap *)
  done
[@@profiling]
(*e: function [[CLI.interpret_bootstrap]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[CLI.main]] *)
let main (caps : <caps; .. >) (argv : string array) : Exit.t =
  let args = ref [] in
  (*s: [[CLI.main()]] debugging initializations *)
  let level = ref (Some Logs.Warning) in
  (*x: [[CLI.main()]] debugging initializations *)
  let action = ref "" in
  (*e: [[CLI.main()]] debugging initializations *)

  let options = [
    (*s: [[CLI.main()]] [[options]] elements *)
    "-i", Arg.Set Flags.interactive,
    " interactive mode (display prompt)";
    "-I", Arg.Clear Flags.interactive,
    " non-interactive mode (no prompt)";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-l", Arg.Set Flags.login,
    " login mode (execute ~/lib/profile)";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-m", Arg.Set_string Flags.rcmain,
    " <file> read commands to initialize rc from file, not /rc/lib/rcmain";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-x", Arg.Set Flags.xflag,
    " print each simple command before executing it";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-s", Arg.Set Flags.sflag,
    " print exit status after any command where the status is non-null";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* pad: I added that *)
    (* TODO: move in a CLI_common.ml *)
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " trace the main functions";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-e", Arg.Set Flags.eflag,
    " exit if $status is non-null after a simple command";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* pad: I added that *)
    "-strict", Arg.Set Flags.strict_mode,
    " strict mode";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-debugger", Arg.Set Flags.debugger,
    " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* pad: I added that *)
    "-dump_tokens", Arg.Set Flags.dump_tokens,
    " dump the tokens as they are generated";
    "-dump_ast", Arg.Set Flags.dump_ast,
    " dump the parsed AST";
    "-dump_opcodes", Arg.Set Flags.dump_opcodes,
    " dump the generated opcodes ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-r", Arg.Set Flags.rflag,
    " print internal form of commands (opcodes)";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* pad: I added that *)
    "-test_parser", Arg.Unit (fun () -> action := "-test_parser"), " ";
    (*e: [[CLI.main()]] [[options]] elements *)
  ]
  in
  (* old: was Arg.parse but we want explicit argv control *)
  (try 
    Arg.parse_argv argv (Arg.align options) (fun t -> args := t::!args) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  (*s: [[CLI.main()]] logging initializations *)
  Logs.set_level !level;
  Logs.info (fun m -> m "ran as %s from %s" argv.(0) (Sys.getcwd ()));
  (*e: [[CLI.main()]] logging initializations *)
  (*s: [[CLI.main()]] CLI action processing *)
  (* to test and debug components of mk *)
  if !action <> "" then begin 
    do_action caps !action (List.rev !args); 
    raise (Exit.ExitCode 0)
  end;
  (*e: [[CLI.main()]] CLI action processing *)
  Var.vinit caps;
  (* todo: trap_init () *)
  (*s: [[CLI.main()]] other initializations *)
  (* todo: 
   * if argc=1 and Isatty then Flags.interactive := true 
   *)
  (*x: [[CLI.main()]] other initializations *)
  (* for 'flags' builtin (see builtin.ml) *)
  argv |> Array.iter (fun s ->
    if s =~ "^-\\([a-zA-Z]\\)"
    then begin
      let letter = Regexp_.matched1 s in
      let char = String.get letter 0 in
      Hashtbl.add Flags.hflags char true
    end
  );
  (*e: [[CLI.main()]] other initializations *)
  try 
    interpret_bootstrap (caps :> < caps >) (List.rev !args);
    Exit.OK
  with exn ->
    (*s: [[CLI.main()]] when [[exn]] thrown in [[interpret()]] *)
    if !Flags.debugger
    then raise exn
    else 
      (match exn with
      (*s: [[CLI.main()]] when [[Failure]] [[exn]] thrown in [[interpret()]] *)
      | Failure s -> 
          (* useful to indicate that error comes from rc, not subprocess *)
          Logs.err (fun m -> m  "rc: %s" s);
          Exit.Code 1
      (*e: [[CLI.main()]] when [[Failure]] [[exn]] thrown in [[interpret()]] *)
      (* alt: could catch Exit.ExitCode here but this is done in Main.ml *)
      | _ -> raise exn
      )
    (*e: [[CLI.main()]] when [[exn]] thrown in [[interpret()]] *)
(*e: function [[CLI.main]] *)
(*e: shell/CLI.ml *)
