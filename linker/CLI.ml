(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

module T = Types

(* for record-building for ocaml-light *)
open Types
open Exec_file

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5l/vl, the Plan 9 ARM/MIPS linkers.
 *
 * Main limitations compared to 5l/vl/...:
 * - no -E digit 
 *   (What was it anyway?)
 * - no optimisation about small data, strings in text section
 *   (really gain?)
 * - no extensions not yet understood (import/export, dynamic linking)
 *   (not sure it was used by any Plan 9 programs)
 * - address of parameter or local is not supported
 *   (Why would you want that? Does 5c generate that?)
 *   update: actually I support it now no?
 *
 * Main limitations compared to 5l:
 * - no half-word specialized instructions and immhalf()
 *   (rare instructions anyway?)
 *
 * Better than 5l/vl/...:
 * - greater code reuse across all linkers thanks to:
 *    * use of marshalling for objects and libraries
 *    * factorized analysis such as Resolve.build_graph, Datagen.gen,
 *      Load.load
 * 
 * todo?:
 *  - -v is quite useful to debug "redefinition" linking errors
 *    (see pb I had when linking bcm/ kernel)
 *  - when get undefined symbol, print function you are currently in!
 *    very useful to diagnose issue to give context and where to look for
 *  - arith LCON less: NCON
 *  - half word and byte load/store basic version
 *  - endianess and datagen
 *  - advanced instructions: floats, MULL, coprocessor, psr, etc
 *  - library ranlib/symdef indexing
 *  - profiling -p
 *  - symbol table
 *  - program counter line table
 *  - nice error reporting for signature conflict, conflicting objects
 *
 * later:
 *  - look at the 5l Go sources in the Golang source, maybe ideas to steal?
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)
(* Need: see .mli *)
type caps = < Cap.open_in; Cap.open_out >

let init_text  = ref None
let init_round = ref None
let init_data  = ref None
(* note that this is not "main"; we give the opportunity to libc _main
 * to do a few things before calling user's main()
 *)
let init_entry = ref "_main"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let config_of_header_type (arch : Arch.t) (header_type : string) : Types.config =
  match header_type with
  | "a.out" | "a.out_plan9" ->
      let header_size = A_out.header_size in
      { 
        header_type = A_out;
        arch;
        header_size;
        init_text  = 
        (match !init_text  with
          | Some x -> x
          | None -> 4096 + header_size
        );
        init_data = !init_data;
        init_round = (match !init_round with Some x -> x | None -> 4096);
        entry_point = !init_entry;
      }
      
  | "elf" | "elf_linux" ->
      let header_size = 
        Int_.rnd (Elf.exec_header_32_size +
                  Elf.nb_program_headers * Elf.program_header_32_size)
                 16
      in
      { 
        header_type = Elf;
        arch;
        header_size;
        init_text  = 
        (match !init_text  with
          | Some x -> x 
          | None -> 0x8000 + header_size
        );
        init_data = !init_data;
        init_round = (match !init_round with Some x -> x | None -> 4096);
        entry_point = !init_entry;
      }
  | s -> failwith (spf "unknown -H option, format not handled: %s" s)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

(* will modify chan as a side effect *)
let link5 (caps : < Cap.open_in; ..> ) (config : T.config) (files : Fpath.t list) (chan : Chan.o) : unit =
  let arch : Ast_asm5.instr_with_cond Arch_linker.t = {
    Arch_linker.branch_opd_of_instr = Ast_asm5.branch_opd_of_instr;
    Arch_linker.visit_globals_instr = Ast_asm5.visit_globals_instr;
  }
  in
  let (code, data, symbols) = Load.load caps files arch in

  (* mark at least as SXref the entry point *)
  T.lookup (config.entry_point, T.Public) None symbols |> ignore;
  Check.check symbols;
  
  let graph = Resolve.build_graph arch.branch_opd_of_instr symbols code in
  let graph = Rewrite5.rewrite graph in

  let symbols2, (data_size, bss_size) = 
    Layout5.layout_data symbols data in
  let symbols2, graph(* why modify that??*), text_size = 
    Layout5.layout_text symbols2 config.init_text graph in

  let sizes : Exec_file.sections_size = { text_size; data_size; bss_size } in
  let init_data =  
    match config.init_data with
    | None -> Int_.rnd (text_size + config.init_text) config.init_round
    | Some x -> x
  in
  let config = { config with init_data = Some init_data } in
  Logs.info (fun m -> m "final config is %s" (Types.show_config config));
 
  let instrs = Codegen5.gen symbols2 config graph in
  let datas  = Datagen.gen symbols2 init_data sizes data in
  Execgen.gen config sizes instrs datas symbols2 chan

let link (caps : < Cap.open_in; ..> ) (arch: Arch.t) (config : T.config) (files : Fpath.t list) (chan : Chan.o) : unit =
  match arch with
  | Arch.Arm ->
     link5 caps config files chan
  | _ -> failwith (spf "TODO: arch not supported yet: %s" (Arch.thestring arch))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : <caps; ..>) (argv : string array) : Exit.t =

  let arch = 
    match Filename.basename argv.(0) with
    | "o5l" -> Arch.Arm
    | "ovl" -> Arch.Mips
    | s -> failwith (spf "arch could not detected from argv0 %s" s)
  in

  let thechar = Arch.thechar arch in
  let thestring = Arch.thestring arch in
  let thebin = spf "%c.out" thechar in
  let usage = 
    spf "usage: %s [-options] objects" argv.(0) 
  in

  let infiles = ref [] in
  let outfile = ref (Fpath.v thebin) in

  (* LATER: detect type depending on current host *)
  let header_type = ref "elf" in

  let level = ref (Some Logs.Warning) in
  (* for debugging *)
  let backtrace = ref false in

  let options = [
    "-o", Arg.String (fun s -> outfile := Fpath.v s),
    spf " <file> output file (default is %s)" !!(!outfile);
    
    "-H", Arg.Set_string header_type,
    spf " <str> executable (header) format (default is %s)" !header_type;
    (* less: Arg support 0x1000 integer syntax? *)
    "-T", Arg.Int (fun i -> init_text := Some i),
    " <addr> start of text section";
    "-R", Arg.Int (fun i -> init_round := Some i),
    " <int> page boundary";
    "-D", Arg.Int (fun i -> init_data := Some i),
    " <addr> start of data section";

    (* less: support integer value instead of string too? *)
    "-E", Arg.Set_string init_entry,
    spf " <str> entry point (default is %s)" !init_entry;

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
  ] |> Arg.align
  in
  (try
    Arg.parse_argv argv options
      (fun f -> infiles := Fpath.v f::!infiles) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  Logs_.setup !level ();
  Logs.info (fun m -> m "linker ran from %s with arch %s" 
        (Sys.getcwd()) thestring);

  (match List.rev !infiles with
  | [] -> 
      Arg.usage options usage; 
      Exit.Code 1
  | xs -> 
        (* sanity checks *)
      (match !init_data, !init_round with
      | Some x, Some y -> 
                failwith (spf "-D%d is ignored because of -R%d" x y)
      | _ -> ()
      );
      let config : Types.config = config_of_header_type arch !header_type in   
      try 
        (* the main call *)
        !outfile |> FS.with_open_out caps (fun chan ->
          link caps arch config xs chan
        );
        (* TODO: set exec bit on outfile *)
        Exit.OK
      with exn ->
       if !backtrace
       then raise exn
       else 
         (match exn with
         | Failure s ->
             Logs.err (fun m -> m "%s" s);
             Exit.Code 1
         (* not sure this exn is currently thrown but just in case *)
         | Location_cpp.Error (s, loc) ->
             (* TODO: actually we should pass locs! *)
             let (file, line) = Location_cpp.final_loc_of_loc loc in
             Logs.err (fun m -> m "%s:%d %s" !!file line s);
             Exit.Code 1
         | _ -> raise exn
         )
  )
