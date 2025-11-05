(*s: Execgen.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types
module T5 = Types5

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Execgen.gen]] *)
let gen (config : Exec_file.linker_config) (sizes : Exec_file.sections_size) (cs : T.word list) (ds : T.byte array) (symbols2 : T.symbol_table2) (chan : Chan.o) : unit =
  let entry_name : string = config.entry_point in
  let entry_addr : T.real_pc =
    try 
      let v = Hashtbl.find symbols2 (entry_name, T.Public) in
      (match v with
      | T.SText2 pc  -> pc
      | _ -> failwith (spf "entry not TEXT: %s" entry_name)
      )
    (* normally impossible if propagated correctly, see main.ml *)
    with Not_found ->
      (* less: 5l does instead default to INITTEXT *)
     failwith (spf "entry not found: %s" entry_name)
  in
  let format = config.header_type in
  Logs.info (fun m -> m "saving executable in %s" (Chan.destination chan));

  match format with
  | Exec_file.A_out ->
      (* Header *)
      A_out.write_header sizes entry_addr chan.oc;

      (* Text section *)
      cs |> List.iter (Endian.Little.output_32 chan.oc);

      (* Data section (no seek to a page boundary; disk image != memory image) *)
      ds |> Array.iter (output_char chan.oc);

      (* todo: symbol table, program counter line table *)
      ()

  | Exec_file.Elf ->
      (* Headers (ELF header + program headers) *)
      let (offset_disk_text, offset_disk_data) = 
        Elf.write_headers config sizes entry_addr chan.oc
      in

      (* bugfix: important seek! we are using Int_.rnd in CLI.ml for
       * header_size and so after the program header we might need
       * some padding, hence this seek.
       *)
      seek_out chan.oc offset_disk_text; (* = config.header_size *)
      (* Text section *)
      cs |> List.iter (Endian.Little.output_32 chan.oc);

      (* Data section *)
      seek_out chan.oc offset_disk_data;
      ds |> Array.iter (output_char chan.oc);

      ()
(*e: function [[Execgen.gen]] *)
(*e: Execgen.ml *)
