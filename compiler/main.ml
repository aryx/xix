(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5c, the Plan 9 C compiler with ARM backend.
 *
 * Main limitations compared to 5c:
 *  - no unicode support
 *  - can not compile multiple files at the same time
 *    (but you should use mk anyway)
 *  - can not compile from stdin
 *    (but who uses that?)
 *  - does not link
 *    (let the linker do that, but 5c does it??)
 *  - no error recovery, stop at first error
 * 
 * todo:
 *  - debugger support
 *  - profiler support
 *  - linker support
 *)

let thechar = '5'
let thestring = "arm"

let usage = 
  spf "usage: %cc [-options] files" thechar

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

let do_action s xs =
  match s with
  | "-test_parser" ->
      xs |> List.iter (fun file ->
        pr2 (spf "processing %s" file);
        let _ = Parse.parse ([], []) file in
        ()
      )

  | _ -> failwith ("action not supported: " ^ s)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)
let compile (defs, include_paths) infile outfile =

  let _ast = Parse.parse (defs, include_paths) infile in
  raise Todo

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main () =

  (* in *)
  let args = ref [] in
  (* out *)
  let outfile = ref "" in

  (* for cpp *)
  let include_paths = ref ["."] in
  let defs = ref [] in
  let include_dot = ref true in

  (* for debugging *)
  let action = ref "" in
  let backtrace = ref false in

  let options = [
    "-o", Arg.Set_string outfile,
    " <file> place output (an object) in file";

    "-D", Arg.String (fun s ->
      let (var, val_) = 
        if s =~ "\\(.*\\)=\\(.*\\)"
        then Common.matched2 s
        else (s, "1")
      in
      Preprocessor.define_cmdline_def (var, val_)
    ), " <name=def> (or just <name>) define the name to the preprocessor";
    "-I", Arg.String (fun s ->
      raise Todo
    ), " <dir> add dir as a path to look for '#include <file>' files";
    "-.", Arg.Clear include_dot,
    " suppress auto search for include files in the file argument's dir";

    (* pad: I used long name for those options *)
    "-debug_inclusion", Arg.Set Flags.debug_inclusion,
    " ";
    "-e", Arg.Set Flags.debug_inclusion,
    " ";

    (* pad: I added that *)
    "-test_parser", Arg.Unit (fun () -> action := "-test_parser"), " ";

    (* pad: I added that *)
    "-dump_tokens", Arg.Set Flags.dump_tokens,
    " dump the tokens as they are generated";
    "-dump_ast", Arg.Set Flags.dump_ast,
    " dump the parsed AST";


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
  (* todo: process the old style -Dname=val and -Idir *)

  (* to test and debug components of mk *)
  if !action <> "" then begin 
    do_action !action (List.rev !args); 
    exit 0 
  end;

  try 
    (match !args, !outfile with
    | [], "" -> 
        Arg.usage (Arg.align options) usage;
        Error.errorexit ""
    | [cfile], outfile ->
        let base = Filename.basename cfile in
        let dir = Filename.dirname cfile in
        let include_paths =
          if !include_dot
          then dir::!include_paths
          else !include_paths
        in
        let include_paths =
          (try
            (* less: split space? *)
            [Sys.getenv "INCLUDE"]
          with Not_found ->
            [spf "/%s/include" thestring; 
             "/sys/include";
            ]
          ) @ include_paths
        in
        
        let outfile = 
          if outfile = ""
          then
            if base =~ "\\(.*\\)\\.c"
            then Common.matched1 base ^ (spf ".%c" thechar)
            else base ^ (spf ".%c" thechar)
          else outfile
        in
        compile (!defs, include_paths) cfile outfile
    | _ -> 
      (* stricter: *)
        failwith 
          "compiling multiple files at the same time is not supported; use mk"
    )
  with exn ->
    if !backtrace || !Flags.debugger
    then raise exn
    else 
      (match exn with
      | Failure s -> 
          (* useful to indicate that error comes from 5c? *)
          Error.errorexit (spf "%cc: %s" thechar s)
      | Error.Error (s, loc) ->
          raise Todo
      | _ -> raise exn
      )

let _ = 
    main ()
