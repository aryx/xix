(*s: Execgen.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Execgen.gen]] *)
let gen (config : Exec_file.linker_config) (sizes : Exec_file.sections_size) (cs : T.byte array) (ds : T.byte array) (symbols2 : T.symbol_table2) (chan : Chan.o) : unit =
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

  (* claude: cs (text) and ds (data) are both flat `byte array`s now --
   * see Types.bytes_of_words's own comment for why (amd64's
   * variable-length instructions can't be flattened into fixed 4-byte
   * words the way every other arch's Codegen*.ml already does). Used
   * to go through Endian.output_functions_of_endian's `output_32`
   * here (each fixed-4-byte-word arch's own Codegen*.gen already
   * picks the right endianness when building its `word list`, then
   * Types.bytes_of_words -- always Little, see its own comment for
   * why -- flattens it to bytes before it ever reaches here). *)
  match format with
  | Exec_file.A_out ->
      (* Header *)
      A_out.write_header config.arch sizes entry_addr chan.oc;

      (* Text section *)
      cs |> Array.iter (output_char chan.oc);

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
      cs |> Array.iter (output_char chan.oc);

      (* Data section *)
      seek_out chan.oc offset_disk_data;
      ds |> Array.iter (output_char chan.oc);

      (* claude: section header table (seeks itself, see Elf.write_sections) *)
      Elf.write_sections config sizes chan.oc;

      ()
(*e: function [[Execgen.gen]] *)
(*e: Execgen.ml *)
