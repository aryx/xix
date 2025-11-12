(*s: CLI.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

module T = Types

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
 * Main limitations compared to vl:
 *  - no sched/nosched support (no scheduling)
 *    (but better not to be too smart? in fact vl was only linker doing that)
 *
 * Better than 5l/vl/...:
 * - greater code reuse across all linkers thanks to:
 *    * use of marshalling for objects and libraries
 *    * factorized analysis such as Resolve.build_graph, Datagen.gen,
 *      Load.load, Layout.layout_data, Profiling.rewrite
 * - less error management code because some states are not possible by
 *   construction (e.g., no need to check for sym in D_EXTERN C case because
 *   Global in OCaml always has a symbol attached)
 * 
 * todo?:
 *  - -v is quite useful to debug "redefinition" linking errors
 *    (see pb I had when linking bcm/ kernel)
 *  - when get undefined symbol, print function you are currently in!
 *    very useful to diagnose issue to give context and where to look for
 *  - library ranlib/symdef indexing
 *  - symbol table
 *  - program counter line table
 *  - nice error reporting for signature conflict, conflicting objects
 * todo 5l:
 *  - arith LCON less: NCON
 *  - half word and byte load/store basic version
 *  - endianess and datagen
 *  - advanced instructions: floats, MULL, coprocessor, psr, etc
 * todo vl:
 *  - a lot
 *
 * later:
 *  - look at the 5l/vl/... Go sources in the Golang source, ideas to steal?
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)
(*s: type [[CLI.caps]] *)
(* Need:
 * - open_in but should be only for argv derived files
 * - open_out for -o exec file or 5.out
 *)
type caps = < Cap.open_in; Cap.open_out >
(*e: type [[CLI.caps]] *)

(*s: constant [[CLI.init_text]] *)
let init_text  = ref None
(*e: constant [[CLI.init_text]] *)
(*s: constant [[CLI.init_round]] *)
let init_round = ref None
(*e: constant [[CLI.init_round]] *)
(*s: constant [[CLI.init_data]] *)
let init_data  = ref None
(*e: constant [[CLI.init_data]] *)

(*s: constant [[CLI.init_entry]] *)
(* note that this is not "main"; we give the opportunity to libc _main
 * to do a few things before calling user's main()
 *)
let init_entry = ref "_main"
(*e: constant [[CLI.init_entry]] *)

let profile : Exec_file.profile_kind option ref = ref None

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[CLI.config_of_header_type]] *)
let config_of_header_type_and_flags (arch : Arch.t) (header_type : string) : Exec_file.linker_config =
  (* sanity checks *)
  (match !init_data, !init_round with
  | Some x, Some y -> failwith (spf "-D%d is ignored because of -R%d" x y)
  | _ -> ()
  );
  let profile = !profile in
  match header_type with
  | "a.out" | "a.out_plan9" ->
      let header_size = A_out.header_size in
      Exec_file.{ 
        header_type = Exec_file.A_out;
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
        profile;
      }
      
  | "elf" | "elf_linux" ->
      let header_size = Elf.header_size in
      Exec_file.{ 
        header_type = Exec_file.Elf;
        arch;
        header_size;
        init_text  = 
        (match !init_text  with
          | Some x -> x 
          | None -> 
              (match arch with
              | Arch.Arm -> 0x8000
              | Arch.Mips -> 0x400000
              | _ -> 
                failwith (spf "arch not supported yet: %s" (Arch.thestring arch))
              ) + header_size
        );
        init_data = !init_data;
        init_round = (match !init_round with Some x -> x | None -> 4096);
        entry_point = !init_entry;
        profile;
      }
  | s -> failwith (spf "unknown -H option, format not handled: %s" s)
(*e: function [[CLI.config_of_header_type]] *)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)
(*s: function [[CLI.link5]] *)
(* will modify chan as a side effect *)
let link5 (caps : < Cap.open_in; ..> ) (config : Exec_file.linker_config) (files : Fpath.t list) (chan : Chan.o) : unit =
  let arch : Ast_asm5.instr_with_cond Arch_linker.t = 
    Arch_linker.of_arch config.arch
  in
  let (code, data, symbols) = Load.load caps files arch in

  (* mark at least as SXref the entry point *)
  T.lookup (config.entry_point, T.Public) None symbols |> ignore;
  
  let graph = Resolve.build_graph arch.branch_opd_of_instr symbols code in
  let graph, new_data =
    match config.profile with
    | None -> graph, []
    | Some kind -> Profile.rewrite kind arch.rTMP symbols graph
  in
  let data = data @ new_data in
  let graph = Rewrite5.rewrite graph in

  let symbols2, (data_size, bss_size) = 
    Layout.layout_data symbols data in
  Layout.xdefine symbols2 symbols ("setR12" , T.Public) (T.SData2 (0, T.Data));

  (* can only check for undefined symbols after layout_data which 
   * can xdefine new symbols (e.g., etext)
   *)
  Check.check symbols;

  let symbols2, graph(* why modify that??*), text_size = 
    Layout5.layout_text symbols2 config.init_text graph in

  let sizes : Exec_file.sections_size = 
    Exec_file.{ text_size; data_size; bss_size } 
  in
  let init_data =  
    match config.init_data with
    | None -> Int_.rnd (text_size + config.init_text) config.init_round
    | Some x -> x
  in
  let config = { config with Exec_file.init_data = Some init_data } in
  Logs.info (fun m -> m "final config is %s" 
        (Exec_file.show_linker_config config));
 
  let instrs = Codegen5.gen symbols2 config graph in
  let datas  = Datagen.gen symbols2 init_data sizes data in
  Execgen.gen config sizes instrs datas symbols2 chan
(*e: function [[CLI.link5]] *)

(* similar to link5 *)
let linkv (caps : < Cap.open_in; ..> ) (config : Exec_file.linker_config) (files : Fpath.t list) (chan : Chan.o) : unit =
  let arch : Ast_asmv.instr Arch_linker.t = Arch_linker.of_arch config.arch in
  let (code, data, symbols) = Load.load caps files arch in
  T.lookup (config.entry_point, T.Public) None symbols |> ignore;
  let graph = Resolve.build_graph arch.branch_opd_of_instr symbols code in
  let graph, new_data =
    match config.profile with
    | None -> graph, []
    | Some kind -> Profile.rewrite kind arch.rTMP symbols graph
  in
  let data = data @ new_data in
  let graph = Rewritev.rewrite graph in
  let symbols2, (data_size, bss_size) = 
    Layout.layout_data symbols data in
  Layout.xdefine symbols2 symbols ("setR30" , T.Public) (T.SData2 (0, T.Data));
  Check.check symbols;
  let symbols2, graph, text_size = 
    Layoutv.layout_text symbols2 config.init_text graph in
  let sizes : Exec_file.sections_size = 
    Exec_file.{ text_size; data_size; bss_size } 
  in
  let init_data =  
    match config.init_data with
    | None -> Int_.rnd (text_size + config.init_text) config.init_round
    | Some x -> x
  in
  let config = { config with Exec_file.init_data = Some init_data } in
  Logs.info (fun m -> m "final config is %s" 
        (Exec_file.show_linker_config config));
  let instrs = Codegenv.gen symbols2 config graph in
  let datas  = Datagen.gen symbols2 init_data sizes data in
  Execgen.gen config sizes instrs datas symbols2 chan


(*s: function [[CLI.link]] *)
let link (caps : < Cap.open_in; ..> ) (arch: Arch.t) (config : Exec_file.linker_config) (files : Fpath.t list) (chan : Chan.o) : unit =
  match arch with
  | Arch.Arm ->
     link5 caps config files chan
  | Arch.Mips ->
     linkv caps config files chan
  | _ -> failwith (spf "TODO: arch not supported yet: %s" (Arch.thestring arch))
(*e: function [[CLI.link]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[CLI.main]] *)
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

    "-p", Arg.Unit (fun () -> profile := Some Exec_file.ProfileTime),
    " profile time spent in a function";
    "-p_time", Arg.Unit (fun () -> profile := Some Exec_file.ProfileTime),
    " profile time spent in a function";
    "-p_count", Arg.Unit (fun () -> profile := Some Exec_file.ProfileCount),
    " profile number of times a function is called";
    "-trace", Arg.Unit (fun () -> profile := Some Exec_file.ProfileTrace),
    " profile and trace when a function is called";

    (* pad: I added that. alt: call Logs_.cli_flags level *)
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
      let config : Exec_file.linker_config = 
          config_of_header_type_and_flags arch !header_type
      in   
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
(*e: function [[CLI.main]] *)
(*e: CLI.ml *)
