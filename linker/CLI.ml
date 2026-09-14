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
 *      Load.load, Layout.layout_data, Profile.rewrite
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
let init_text : T.addr option ref  = ref None
(*e: constant [[CLI.init_text]] *)
(*s: constant [[CLI.init_round]] *)
let init_round : int option ref = ref None
(*e: constant [[CLI.init_round]] *)
(*s: constant [[CLI.init_data]] *)
let init_data : T.addr option ref  = ref None
(*e: constant [[CLI.init_data]] *)

(*s: constant [[CLI.init_entry]] *)
let init_entry : string option ref = ref None
(*e: constant [[CLI.init_entry]] *)

(* note that this is not "main"; we give the opportunity to libc _main
 * to do a few things before calling user's main()
 *)
let default_entry_point = "_main"

let profile : Exec_file.profile_kind option ref = ref None


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[CLI.config_of_header_type]] *)
let config_of_header_type_and_flags (arch : Arch.t) (header_type : string) :
    Exec_file.linker_config =
  (* sanity checks *)
  (match !init_data, !init_round with
  | Some x, Some y -> failwith (spf "-D%d is ignored because of -R%d" x y)
  | _ -> ()
  );
  let entry_point =
    match !init_entry, !profile with
    | None, None -> default_entry_point
    | None, Some _ -> "_mainp"
    | Some s, _ -> s
  in

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
        entry_point;
        profile;
      }
      
  | "elf" | "elf_linux" ->
      let header_size = Elf.header_size (Arch.bits_of_arch arch) in
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
              (* claude: same default INITTEXT for riscv64 -- goken's
               * il/obj.c picks INITTEXT from HEADTYPE only, not
               * thechar, see mkfiles/riscv64/mkfile's "ja/jc/jl are
               * the *same* binaries as ia/ic/il" comment *)
              | Arch.Riscv | Arch.Riscv64 -> 0x10000
              (* claude: matches goken's own default (confirmed
               * empirically: entry point 0x4000f0 = 0x400000 + a
               * 0xf0-byte ELF header) *)
              | Arch.Arm64 -> 0x400000
              (* claude: matches goken's own real 6l default (confirmed
               * empirically: entry point 0x2000f0 = 0x200000 + a
               * 0xf0-byte ELF header, via readelf on goken's own
               * hello_linux_amd64.exe). *)
              | Arch.Amd64 -> 0x200000
              | _ ->
                failwith (spf "arch not supported yet: %s" (Arch.thestring arch))
              ) + header_size
        );
        init_data = !init_data;
        init_round = (match !init_round with Some x -> x | None -> 4096);
        entry_point;
        profile;
      }
  | s -> failwith (spf "unknown -H option, format not handled: %s" s)
(*e: function [[CLI.config_of_header_type]] *)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)
(*s: function [[CLI.link5]] *)
(* will modify chan as a side effect *)
let link5 (caps : < Cap.open_in; ..> ) (config : Exec_file.linker_config)
    (files : Fpath.t list) (chan : Chan.o) :
    unit =
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

  (* arch-specific phase *)
  let graph = Rewrite5.rewrite graph in
  (* TODO? Optimize5.rewrite ? like ADD -N => SUB N *)

  let symbols2, (data_size, bss_size) =
    Layout.layout_data symbols data in
  (* claude: setR12 must sit Codegen5.big (goken's BIG = 4092) bytes
   * into the data segment, not at offset 0 -- goken's own layout.c:
   * "xdefine(setR12, SDATA, 0L+BIG)". R12 is loaded with this address
   * at process startup (rt0.s's "MOVW $setR12(SB), R12") and every
   * later sym(SB) access through R12 is encoded by Codegen5's
   * offset_to_R12 (x - big) assuming that bias -- offset 0 here was
   * silently wrong by exactly `big` bytes for every SB-relative data
   * access, undetected until a real multi-object closure (hello_libc)
   * exercised any sym(SB) reference outside a single small test
   * fixture's own object: found as a SIGSEGV writing 4092 bytes
   * before _mainargv, see docs/claude_notes/plan_hello_libc_linking.md. *)
  Layout.xdefine symbols2 symbols ("setR12" , T.Public) (T.SData2 (Codegen5.big, T.Data));

  (* can only check for undefined symbols after layout_data which 
   * can xdefine new symbols (e.g., etext)
   *)
  Check.check symbols;

  (* arch-specific phase *)
  let symbols2, graph(* why return it? modified ?? *), text_size = 
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
 
  (* arch-specific phase *)
  let instrs = Codegen5.gen symbols2 config graph in

  let endian = Arch.endian_of_arch config.arch in
  let instrs = T.bytes_of_words endian instrs in
  let datas  = Datagen.gen symbols2 init_data sizes endian data in
  Execgen.gen config sizes instrs datas symbols2 chan
(*e: function [[CLI.link5]] *)

(* similar to link5 *)
let linkv (caps : < Cap.open_in; ..> ) (config : Exec_file.linker_config) (files : Fpath.t list) (chan : Chan.o) : unit =
  let arch : Ast_asmv.instr Arch_linker.t = Arch_linker.of_arch config.arch in
  let (code, data, symbols) = Load.load caps files arch in
  T.lookup (config.entry_point, T.Public) None symbols |> ignore;
  let graph = Resolve.build_graph arch.branch_opd_of_instr symbols code in
  let graph, new_data =
    match config.profile with | None -> graph, []
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
  let sizes = Exec_file.{ text_size; data_size; bss_size } in
  let init_data =  
    match config.init_data with | Some x -> x
    | None -> Int_.rnd (text_size + config.init_text) config.init_round
  in
  let config = { config with Exec_file.init_data = Some init_data } in
  Logs.info (fun m -> m "final config is %s" 
        (Exec_file.show_linker_config config));
  let instrs = Codegenv.gen symbols2 config graph in
  let endian = Arch.endian_of_arch config.arch in
  let instrs = T.bytes_of_words endian instrs in
  let datas  = Datagen.gen symbols2 init_data sizes endian data in
  Execgen.gen config sizes instrs datas symbols2 chan

(* claude: new function, mirroring link5/linkv (which I didn't write) *)
let linki (caps : < Cap.open_in; ..> ) (config : Exec_file.linker_config) (files : Fpath.t list) (chan : Chan.o) : unit =
  let arch : Ast_asmi.instr Arch_linker.t = Arch_linker.of_arch config.arch in
  let (code, data, symbols) = Load.load caps files arch in
  T.lookup (config.entry_point, T.Public) None symbols |> ignore;
  let graph = Resolve.build_graph arch.branch_opd_of_instr symbols code in
  let graph, new_data =
    match config.profile with | None -> graph, []
    | Some kind -> Profile.rewrite kind arch.rTMP symbols graph
  in
  let data = data @ new_data in
  let is_64 = (match config.arch with Arch.Riscv64 -> true | _ -> false) in
  let graph = Rewritei.rewrite is_64 graph in
  let symbols2, (data_size, bss_size) =
    Layout.layout_data symbols data in
  (* claude: setSB's value is BIG, not 0 -- see Codegeni.ml's `big`
   * for why this matters on RISC-V (unlike ARM's setR12, where the
   * choice of 0 vs BIG happens to not matter for the fixtures so
   * far -- see docs/claude_notes/riscv_port.md). *)
  Layout.xdefine symbols2 symbols ("setSB" , T.Public)
    (T.SData2 (Codegeni.big, T.Data));
  Check.check symbols;
  let symbols2, graph, text_size =
    Layouti.layout_text symbols2 config.init_text graph in
  let sizes = Exec_file.{ text_size; data_size; bss_size } in
  let init_data =
    match config.init_data with | Some x -> x
    | None -> Int_.rnd (text_size + config.init_text) config.init_round
  in
  let config = { config with Exec_file.init_data = Some init_data } in
  Logs.info (fun m -> m "final config is %s"
        (Exec_file.show_linker_config config));
  let instrs = Codegeni.gen symbols2 config graph in
  let endian = Arch.endian_of_arch config.arch in
  let instrs = T.bytes_of_words endian instrs in
  let datas  = Datagen.gen symbols2 init_data sizes endian data in
  Execgen.gen config sizes instrs datas symbols2 chan

(* claude: mirrors link5/linkv/linki (which I didn't write) *)
let link7 (caps : < Cap.open_in; ..> ) (config : Exec_file.linker_config) (files : Fpath.t list) (chan : Chan.o) : unit =
  let arch : Ast_asm7.instr Arch_linker.t = Arch_linker.of_arch config.arch in
  let (code, data, symbols) = Load.load caps files arch in
  T.lookup (config.entry_point, T.Public) None symbols |> ignore;
  let graph = Resolve.build_graph arch.branch_opd_of_instr symbols code in
  let graph, new_data =
    match config.profile with | None -> graph, []
    | Some kind -> Profile.rewrite kind arch.rTMP symbols graph
  in
  let data = data @ new_data in
  let graph = Rewrite7.rewrite graph in
  let symbols2, (data_size, bss_size) =
    Layout.layout_data symbols data in
  (* claude: REGSB (x28) points at data-offset 0 with no bias at all
   * (confirmed empirically -- see Ast_asm7.ml's prelude comment),
   * unlike ARM32's/RISC-V's BIG-biased SB register. *)
  Layout.xdefine symbols2 symbols ("setSB" , T.Public) (T.SData2 (0, T.Data));
  Check.check symbols;
  let symbols2, graph, text_size =
    Layout7.layout_text symbols2 config.init_text graph in
  let sizes = Exec_file.{ text_size; data_size; bss_size } in
  let init_data =
    match config.init_data with | Some x -> x
    | None -> Int_.rnd (text_size + config.init_text) config.init_round
  in
  let config = { config with Exec_file.init_data = Some init_data } in
  Logs.info (fun m -> m "final config is %s"
        (Exec_file.show_linker_config config));
  let instrs = Codegen7.gen symbols2 config graph in
  let endian = Arch.endian_of_arch config.arch in
  let instrs = T.bytes_of_words endian instrs in
  let datas  = Datagen.gen symbols2 init_data sizes endian data in
  Execgen.gen config sizes instrs datas symbols2 chan

(* claude: no `Layout.xdefine ... "setSB"` here (unlike link5/linkv/
 * link7) -- this arch has no dedicated SB register at all: SB-relative
 * addressing (globals, via `Lea`/`Move`'s `Entity (A.Global ...)`
 * case) resolves straight to an absolute virtual address at codegen
 * time (Codegen6.ml's `resolve_gen`/`resolve_gen_full`), the same
 * "goken's own non-PIE amd64 convention" this port's whole SB-handling
 * story is grounded in -- see Ast_asm6.ml's prelude. *)
let link6 (caps : < Cap.open_in; ..> ) (config : Exec_file.linker_config) (files : Fpath.t list) (chan : Chan.o) : unit =
  let arch : Ast_asm6.instr Arch_linker.t = Arch_linker.of_arch config.arch in
  let (code, data, symbols) = Load.load caps files arch in
  T.lookup (config.entry_point, T.Public) None symbols |> ignore;
  let graph = Resolve.build_graph arch.branch_opd_of_instr symbols code in
  let graph, new_data =
    match config.profile with | None -> graph, []
    | Some kind -> Profile.rewrite kind arch.rTMP symbols graph
  in
  let data = data @ new_data in
  let graph = Rewrite6.rewrite graph in
  let symbols2, (data_size, bss_size) =
    Layout.layout_data symbols data in
  Check.check symbols;
  let symbols2, graph, text_size =
    Layout6.layout_text symbols2 config.init_text graph in
  let sizes = Exec_file.{ text_size; data_size; bss_size } in
  let init_data =
    match config.init_data with | Some x -> x
    | None -> Int_.rnd (text_size + config.init_text) config.init_round
  in
  let config = { config with Exec_file.init_data = Some init_data } in
  Logs.info (fun m -> m "final config is %s"
        (Exec_file.show_linker_config config));
  (* claude: Codegen6.gen already returns a `byte array` directly
   * (amd64's instructions are variable-length -- see Types.
   * bytes_of_words's own comment), unlike every other arch's `word
   * list`, so there's no T.bytes_of_words conversion step here. *)
  let instrs = Codegen6.gen symbols2 config graph in
  let endian = Arch.endian_of_arch config.arch in
  let datas  = Datagen.gen symbols2 init_data sizes endian data in
  Execgen.gen config sizes instrs datas symbols2 chan

(*s: function [[CLI.link]] *)
let link (caps : < Cap.open_in; ..> ) (arch: Arch.t) (config : Exec_file.linker_config) (files : Fpath.t list) (chan : Chan.o) : unit =
  match arch with
  | Arch.Arm ->
     link5 caps config files chan
  | Arch.Mips ->
     linkv caps config files chan
  | Arch.Arm64 ->
     link7 caps config files chan
  | Arch.Amd64 ->
     link6 caps config files chan
  (* claude: riscv64/ojl reuses linki as-is, mirroring goken itself --
   * il/jl are literally the same binary (thechar dispatches on argv0
   * at runtime, see mkfiles/riscv64/mkfile); none of Codegeni.ml,
   * Rewritei.ml, Layouti.ml reference Arch.t at all, so they're
   * already arch-width-agnostic. Only Elf.ml (ELF32 vs ELF64) and
   * this dispatch needed to change. *)
  | Arch.Riscv | Arch.Riscv64 ->
     linki caps config files chan
  | _ -> failwith (spf "TODO: arch not supported yet: %s" (Arch.thestring arch))
(*e: function [[CLI.link]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[CLI.main]] *)
let main (caps : <caps; Cap.stdout; Cap.stderr; ..>) (argv : string array) :
    Exit.t =

  let arch = 
    match Filename.basename argv.(0) with
    | "o5l" -> Arch.Arm
    | "ovl" -> Arch.Mips
    | "oil" -> Arch.Riscv
    | "ojl" -> Arch.Riscv64
    | "o7l" -> Arch.Arm64
    | "o6l" -> Arch.Amd64
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
    "-E", Arg.String (fun s -> init_entry := Some s),
    spf " <str> entry point (default is %s)" default_entry_point;

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

    (* claude: ARM-only (matches goken's 5l -f); a no-op for other
     * archs, same as how goken's own vl/il/... don't have this flag
     * at all -- kept here since CLI.ml's option list is shared
     * across archs, like -debug_layout/-debug_gen above. *)
    "-f", Arg.Set Flags.vfp,
    " (ARM-only) use VFP float instructions instead of the legacy FPA ones";
  ] |> Arg.align
  in
  (* This may raise ExitCode *)
  Arg_.parse_argv caps argv options
      (fun f -> infiles := Fpath.v f::!infiles) usage;
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
       let outfile = !outfile in
       if Sys.file_exists !!outfile
       then begin 
         Logs.info (fun m -> m "removing %s because of error" !!outfile);
         FS.remove caps outfile;
       end;
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
