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

let thechar = '5'
let usage = 
  spf "usage: %cl [-options] objects" thechar

let link config objfiles outfile =
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

  let sizes = { T.text_size; data_size; bss_size } in
  let init_data =  
    match config.T.init_data with
    | None -> rnd (text_size + config.T.init_text) config.T.init_round
    | Some x -> x
  in
  let config = { config with T.init_data = Some init_data } in
 
  let instrs = Codegen5.gen symbols2 config graph in
  let datas  = Datagen.gen symbols2 init_data sizes data in
  Executable.gen config sizes instrs datas symbols2 outfile

let main () =
  let infiles = ref [] in
  let outfile = ref "5.out" in

  let header_type = ref "a.out" in
  let init_text  = ref None in
  let init_round = ref None in
  let init_data  = ref None in
  let init_entry = ref "_main" in

  let options = [
    "-o", Arg.Set_string outfile,
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

    "-debug_layout", Arg.Set Flags.debug_layout,
    " debug layout code";
    "-debug_gen", Arg.Set Flags.debug_gen,
    " debug code generation";
  ]
  in
  Arg.parse options (fun f -> infiles := f::!infiles) usage;

  (match List.rev !infiles with
  | [] -> 
      Arg.usage options usage; 
      exit (-1)
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

      (* the main call *)
      link config xs !outfile
  )

let _ = 
  main ()
