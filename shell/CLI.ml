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

(* see the .mli for why those caps are needed
 * TODO? could remove Cap.exit and use Exit.ExitCode exn in Process.ml instead
 *)
type caps = < Cap.fork; Cap.exec; Cap.chdir; Cap.env; Cap.exit; Cap.open_in >

(* -d and -p are dead according to man page so I removed them *)
let usage =
  "usage: rc [-SsriIlxevV] [-c arg] [-m command] [file [arg ...]]"

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let _bootstrap_simple : O.opcode array = 
  [| O.F O.REPL |]

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


let interpret (caps : < caps >) (args : string list) : unit =
  let t = R.mk_thread (bootstrap ()) 0 (Hashtbl.create 11) in
  R.runq := t::!R.runq;

  t.R.lexbuf <- Lexing.from_channel stdin;
  t.R.iflag <- !Flags.interactive;

  (* less: set argv0 *)
  args |> List.rev |> List.iter Runtime.push_word;

  while true do

    (* bugfix: need to fetch the current thread each time,
     * as the interpreted code may have modified runq.
     *)
    let t = Runtime.cur () in
    let pc = t.R.pc in

    (* less: cycle =~ codevec pointer *)
    if !Flags.rflag
    then Logs.app (fun m -> m "pid %d %d %s %s"
                (Unix.getpid ())
                !pc
                (Dumper_.s_of_opcode t.R.code.(!pc))
                ( (t.R.argv::t.R.argv_stack) |> List.map (fun xs ->
                    spf "(%s)" (String.concat " " xs)
                   ) |> String.concat " "));
    
    incr pc;
    (match t.R.code.(!pc - 1) with

    (* opcode dispatch ! *)
    | O.F operation ->  Interpreter.interpret caps operation
    | O.S s -> failwith (spf "was expecting a F, not a S: %s" s)
    | O.I i -> failwith (spf "was expecting a F, not a I: %d" i)
    );
    (* todo: handle trap *)
  done
[@@profiling]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : <caps; .. >) (argv : string array) : Exit.t =

  let args = ref [] in

  (* for debugging *)
  let level = ref (Some Logs.Warning) in
  let action = ref "" in

  let options = [
    "-i", Arg.Set Flags.interactive,
    " interactive mode (display prompt)";
    "-I", Arg.Clear Flags.interactive,
    " non-interactive mode (no prompt)";
    "-l", Arg.Set Flags.login,
    " login mode (execute ~/lib/profile)";
    "-m", Arg.Set_string Flags.rcmain,
    " <file> read commands to initialize rc from file, not /rc/lib/rcmain";

    "-e", Arg.Set Flags.eflag,
    " exit if $status is non-null after a simple command";

    "-r", Arg.Set Flags.rflag,
    " print internal form of commands (opcodes)";
    "-s", Arg.Set Flags.sflag,
    " print exit status after any command where the status is non-null";
    "-x", Arg.Set Flags.xflag,
    " print each simple command before executing it";

    (* pad: I added that *)
    "-strict", Arg.Set Flags.strict_mode,
    " strict mode";

    (* pad: I added that *)
    "-test_parser", Arg.Unit (fun () -> action := "-test_parser"), " ";

    (* pad: I added that *)
    "-dump_tokens", Arg.Set Flags.dump_tokens,
    " dump the tokens as they are generated";
    "-dump_ast", Arg.Set Flags.dump_ast,
    " dump the parsed AST";
    "-dump_opcodes", Arg.Set Flags.dump_opcodes,
    " dump the generated opcodes ";

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
    "-debugger", Arg.Set Flags.debugger,
    " ";
  ]
  in
  (* old: was Arg.parse but we want explicit argv control *)
  (try 
    Arg.parse_argv argv (Arg.align options) (fun t -> args := t::!args) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );


  Logs.set_level !level;
  Logs.info (fun m -> m "ran as %s from %s" argv.(0) (Sys.getcwd ()));

  (* to test and debug components of mk *)
  if !action <> "" then begin 
    do_action caps !action (List.rev !args); 
    raise (Exit.ExitCode 0)
  end;

  (* todo: 
   * if argc=1 and Isatty then Flags.interactive := true 
   *)
  Var.vinit caps;
  (* todo: trap_init () *)

  (* for 'flags' builtin (see builtin.ml) *)
  argv |> Array.iter (fun s ->
    if s =~ "^-\\([a-zA-Z]\\)"
    then begin
      let letter = Regexp_.matched1 s in
      let char = String.get letter 0 in
      Hashtbl.add Flags.hflags char true
    end
  );

  try 
    interpret (caps :> < caps >) (List.rev !args);
    Exit.OK
  with exn ->
    if !Flags.debugger
    then raise exn
    else 
      (match exn with
      | Failure s -> 
          (* useful to indicate that error comes from rc, not subprocess *)
          Logs.err (fun m -> m  "rc: %s" s);
          Exit.Code 1
      (* alt: could catch Exit.ExitCode here but this is done in Main.ml *)
      | _ -> raise exn
      )
