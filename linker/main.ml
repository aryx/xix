(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5l, the plan9 ARM linker.
 *
 * Limitations compared to 5l:
 * - the -E digit
 * 
 * todo?:
 *  - profiling -p
 *  - symbol table, program counter line table
 *  - nice error reporting for signature conflict
 *  - library input files, libpath
 *)

let thechar = '5'
let usage = 
  spf "usage: %cl [-options] objects" thechar

let link config xs outfile =
  let (code, data, symbols) = Load5.load xs in

  (* mark at least as SXref the entry point *)
  T.lookup (config.T.entry_point, T.Public) None symbols |> ignore;
  Check.check symbols;
  
  let graph = Resolve5.build_graph symbols code in
  let graph = Rewrite5.rewrite graph in

  let symbols2, (data_size, bss_size) = 
    Resolve5.layout_data symbols in
  let symbols2, text_size = 
    Resolve5.layout_text symbols2 config.T.init_text graph in

  let sizes = { T.text_size; data_size; bss_size } in
  (* todo: modify config now that can know initdat *)
 
  let instrs = Codegen5.gen symbols2 config graph in
  let datas = Datagen.gen symbols2 data in
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
    "-T", Arg.Int (fun i -> init_text := Some i),
    " <addr> start of text section";
    "-R", Arg.Int (fun i -> init_round := Some i),
    " <int> page boundary";
    "-D", Arg.Int (fun i -> init_data := Some i),
    " <addr> start of data section";

    "-E", Arg.Set_string init_entry,
    " <str> entry point";
  ]
  in
  Arg.parse options (fun f -> infiles := f::!infiles) usage;

  (match !infiles with
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
              init_text  = (match !init_text  with Some x -> x | None -> 4128);
              init_round = (match !init_round with Some x -> x | None -> 4096);
              init_data = !init_data;
              entry_point = !init_entry;
            }
              
        | "elf" -> raise Todo
        | s -> failwith (spf "unknown -H option, format not handled: %s" s)
      in
      link config xs !outfile
  )
  

let _ = 
  main ()
