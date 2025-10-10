(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5l, the Plan 9 ARM linker.
 *
 * Main limitations compared to 5l:
 * - no -E digit 
 *   (What was it anyway?)
 * - no optimisation about small data, strings in text section
 *   (really gain?)
 * - no extensions not yet understood (import/export, dynamic linking)
 *   (not sure it was used by any Plan 9 programs)
 * - no half-word specialized instructions and immhalf()
 *   (rare instructions anyway?)
 * - address of parameter or local is not supported
 *   (Why would you want that? Does 5c generate that?)
 *   update: actually I support it now no?
 * 
 * todo?:
 *  - look at the 5l Go sources in the Golang source, maybe ideas to steal?
 *  - -v is quite useful to debug "redefinition" linking errors
 *    (see pb I had when linking bcm/ kernel)
 *  - when get undefined symbol, print function you are currently in!
 *    very useful to diagnose issue to give context and where to look for
 *  - arith LCON less: NCON
 *  - half word and byte load/store basic version
 *  - endianess and datagen
 *  - advanced instructions: floats, MULL, coprocessor, psr, etc
 *  - library, but need 5c in ocaml first I think, libpath
 *  - profiling -p
 *  - symbol table
 *  - program counter line table
 *  - nice error reporting for signature conflict, conflicting objects
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)
(* Need: see .mli *)
type caps = < >

let thechar = '5'
let usage = 
  spf "usage: %cl [-options] objects" thechar

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let link config (objfiles : Fpath.t list) outfile =
  let (code, data, symbols) = Load5.load objfiles in

  (* mark at least as SXref the entry point *)
  T.lookup (config.T.entry_point, T.Public) None symbols |> ignore;
  Check.check symbols;
  
  let graph = Resolve5.build_graph symbols code in
  let graph = Rewrite5.rewrite graph in

  let symbols2, (data_size, bss_size) = 
    Layout5.layout_data symbols data in
  let symbols2, graph(* why modify that??*), text_size = 
    Layout5.layout_text symbols2 config.T.init_text graph in

  let sizes = { T.text_size = text_size; data_size = data_size; bss_size = bss_size } in
  let init_data =  
    match config.T.init_data with
    | None -> Int_.rnd (text_size + config.T.init_text) config.T.init_round
    | Some x -> x
  in
  let config = { config with T.init_data = Some init_data } in
 
  let instrs = Codegen5.gen symbols2 config graph in
  let datas  = Datagen.gen symbols2 init_data sizes data in
  Executable.gen config sizes instrs datas symbols2 outfile

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (_caps : <caps; ..>) (argv : string array) : Exit.t =
  let infiles = ref [] in
  let outfile = ref (Fpath.v "5.out") in

  let header_type = ref "a.out" in
  let init_text  = ref None in
  let init_round = ref None in
  let init_data  = ref None in
  let init_entry = ref "_main" in

  let level = ref (Some Logs.Warning) in
  (* for debugging *)
  let backtrace = ref false in

  let options = [
    "-o", Arg.String (fun s -> outfile := Fpath.v s),
    " <file> output file";
    
    "-H", Arg.Set_string header_type,
    " <str> executable format";
    (* less: Arg support 0x1000 integer syntax? *)
    "-T", Arg.Int (fun i -> init_text := Some i),
    " <addr> start of text section";
    "-R", Arg.Int (fun i -> init_round := Some i),
    " <int> page boundary";
    "-D", Arg.Int (fun i -> init_data := Some i),
    " <addr> start of data section";

    (* less: support integer value instead of string too? *)
    "-E", Arg.Set_string init_entry,
    " <str> entry point";

    (* pad: I added that *)
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " guess what";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";

    (* pad: I added that *)
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";

    "-debug_layout", Arg.Set Flags.debug_layout,
    " debug layout code";
    "-debug_gen", Arg.Set Flags.debug_gen,
    " debug code generation";
  ]
  in
  (try
    Arg.parse_argv argv options (fun f -> infiles := Fpath.v f::!infiles) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  Logs_.setup !level ();
  Logs.info (fun m -> m "5a ran from %s" (Sys.getcwd()));

  (match List.rev !infiles with
  | [] -> 
      Arg.usage options usage; 
      raise (Exit.ExitCode 1)
  | xs -> 
      let config = 
        match !header_type with
        | "a.out" ->
            (match !init_data, !init_round with
            | Some x, Some y -> 
                failwith (spf "-D%d is ignored because of -R%d" x y)
            | _ -> ()
            );
            { T.
              header_type = "a.out";
              header_size = 32;
              (* 4128 = 4096 (1 page) + 32 (the header) *)
              init_text  = (match !init_text  with Some x -> x | None -> 4128);
              init_round = (match !init_round with Some x -> x | None -> 4096);
              init_data = !init_data;
              entry_point = !init_entry;
            }
              
        | "elf" -> raise Todo
        | s -> failwith (spf "unknown -H option, format not handled: %s" s)
      in
     try 
        (* the main call *)
        link config xs !outfile;
        Exit.OK
  with exn ->
    if !backtrace
    then raise exn
    else 
      (match exn with
(*
      | Location_cpp.Error (s, loc) ->
          (* less: could use final_loc_and_includers_of_loc loc *)
          let (file, line) = Location_cpp.final_loc_of_loc loc in
          Logs.err (fun m -> m "%s:%d %s" !!file line s);
          Exit.Code 1
*)
      | _ -> raise exn
      )
  )
