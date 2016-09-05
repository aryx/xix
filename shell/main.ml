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
 *  - no unicode
 *  - no fancy redirection or pipe
 * 
 * 
 * todo:
 *  - Isatty
 *  - add ~ shortcut for HOME
 *)

let usage =
  "usage: rc [-SsrdiIlxepvV] [-c arg] [-m command] [file [arg ...]]"


(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

let do_action s xs =
  match s with
  | _ -> failwith ("action not supported: " ^ s)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let bootstrap_simple = 
  [| Opcode.F Op_repl.op_repl|]

(* the real one is more complex *)
let bootstrap = 
  [| |]


let interpreter () =
  Runtime.start bootstrap_simple 0 (Hashtbl.create 11);

  let t = Runtime.cur () in
  t.R.chan <- stdin;
  t.R.iflag <- !Flags.interactive;

  (* less: set argv0 *)
  for i = Array.length Sys.argv - 1 to 1 do
    Runtime.push_word (Sys.argv.(i))
  done;

  while true do
    (* less: debug runq *)
    incr t.R.pc;
    (match t.R.code.(!(t.R.pc) - 1) with
    | O.F f -> f ()
    | O.S s -> 
        failwith (spf "was expecting a F, not a S: %s" s)
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
    "-l", Arg.Set Flags.login,
    " login mode (execute ~/lib/profile)";

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

