(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module R = Runtime
module O = Opcode

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of rc, the Plan9 shell.
 *
 * Main limitations compared to rc:
 *  - no unicode support
 *  - not all the fancy redirections and fancy pipes
 *  - no storing of function in the environment
 *    (used by rcmain. But can do the same by using '. rcmain')
 * 
 * todo:
 *  - Isatty
 *  - add ~ shortcut for HOME (from csh?)
 *  - rc foo.rc
 *  - rc -c
 *  - rcmain and rc -m 
 *)

let usage =
  "usage: rc [-SsriIlxevV] [-c arg] [-m command] [file [arg ...]]"
(* -d and -p are dead according to man page so I removed them *)

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

let do_action s xs =
  match s with
  | "-test_parser" ->
      xs |> List.iter (fun file ->
        pr2 (spf "processing %s" file);
        let chan = open_in file in
        let lexbuf = Lexing.from_channel chan in

        (* for error reporting I need a runq *)
        Runtime.start [||] 0 (Hashtbl.create 0);
        let t = Runtime.cur () in
        t.R.file <- Some file;

        let rec loop () =
          let line = Parse.parse_line lexbuf in
          match line with
          | Some seq -> 
            pr2 (Dumper.s_of_cmd_sequence seq);
            loop ();
          | None -> ()
        in
        loop ()
      )

  | _ -> failwith ("action not supported: " ^ s)

(*****************************************************************************)
(* Opcode dispatcher *)
(*****************************************************************************)

let dispatch operation =
  match operation with
  | O.REPL -> Op_repl.op_REPL ()
  | O.Simple -> Op_process.op_Simple ()

  (* push_list() *)
  | O.Mark -> 
      let t = R.cur () in
      t.R.argv_stack <- t.R.argv :: t.R.argv_stack;
      t.R.argv <- [];

  | O.Word ->
      let t = R.cur () in
      let x = t.R.code.(!(t.R.pc)) in
      incr t.R.pc;
      (match x with
      | O.S s -> R.push_word s
      (* stricter *)
      | _ -> failwith (spf "was expecting a S, not %s" 
                         (Dumper.s_of_operation operation))
      )

  | _ -> failwith ("TODO: " ^ Dumper.s_of_opcode (O.F operation))

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let bootstrap_simple = 
  [| O.F O.REPL |]

(* The real one is more complex.
 * *=(argv);. /usr/lib/rcmain $*
 *)
let bootstrap = 
  [| 
      O.F O.Mark;

      O.F O.Word;
      O.S "*";
      O.F O.Assign;
      O.F O.Mark;

      O.F O.Mark;

      O.F O.Word;
      O.S "*";
      O.F O.Dollar;

      O.F O.Word;
      O.S "/rc/lib/rcmain"; (* or use -m *)

      O.F O.Word;
      O.S ".";

      O.F O.Simple;

      O.F O.Exit;
  |]


let interpreter () =
  Runtime.start bootstrap_simple 0 (Hashtbl.create 11);

  let t = Runtime.cur () in
  t.R.chan <- stdin;
  t.R.iflag <- !Flags.interactive;

  (* less: set argv0 *)
  for i = (Array.length Sys.argv) - 1 downto 1 do
    Runtime.push_word (Sys.argv.(i))
  done;

  while true do

    (* bug: need to fetch the current thread each time,
     * as the interpreted code may have modified runq.
     *)
    let t = Runtime.cur () in

    (* less: debug runq *)
    incr t.R.pc;
    (match t.R.code.(!(t.R.pc) - 1) with

    (* opcode dispatch ! *)
    | O.F operation ->  dispatch operation
    | O.S s -> failwith (spf "was expecting a F, not a S: %s" s)
    | O.I i -> failwith (spf "was expecting a F, not a I: %d" i)
    );
    (* todo: handle trap *)
  done


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main () =

  let args = ref [] in

  (* for debugging *)
  let action = ref "" in
  let backtrace = ref false in

  let options = [
    "-i", Arg.Set Flags.interactive,
    " interactive mode (display prompt)";
    "-I", Arg.Clear Flags.interactive,
    " non-interactive mode (no prompt)";
    "-l", Arg.Set Flags.login,
    " login mode (execute ~/lib/profile)";
    "-e", Arg.Set Flags.eflag,
    " exit if $status is non-null after a simple command";

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
    "-debugger", Arg.Set Flags.debugger,
    " ";
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";

  ]
  in
  Arg.parse (Arg.align options) (fun t -> 
    args := t::!args
  ) usage;

  (* to test and debug components of mk *)
  if !action <> "" then begin 
    do_action !action (List.rev !args); 
    exit 0 
  end;

  (* less: if argc=1 and Isatty then Flags.interactive := true *)
  Var.vinit ();
  (* todo: trap_init () *)

  try 
    interpreter ()
  with exn ->
    if !backtrace || !Flags.debugger
    then raise exn
    else 
      (match exn with
      | Failure s -> 
          (* useful to indicate that error comes from rc, not subprocess *)
          pr2 ("rc: " ^ s);
          exit (1)
      | _ -> raise exn
      )

let _ = 
    main ()
