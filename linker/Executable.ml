(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(* for record-building for ocaml-light *)
open A_out 

module T = Types
module T5 = Types5

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
  
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let gen (config : T.config) (sizes : T.sections_size) (cs : T.word list) (ds : T.byte array) (symbols2 : T.symbol_table2) (chan : Chan.o) : unit =
  let entry = config.T.entry_point in
  let format = config.T.header_type in

  (match format with
  | Types.A_out -> ()
  | Types.Elf -> failwith "ELF executable format not yet supported"
  );

  let header = { A_out.
     (* Plan 9 ARM *)
     magic = 0x647;

     text_size = sizes.T.text_size;
     data_size = sizes.T.data_size;
     bss_size = sizes.T.bss_size;

     (* todo: for now stripped *)
     symbol_size = 0;
     pc_size = 0;
     
     entry =
      try 
        let v = Hashtbl.find symbols2 (entry, T.Public) in
        (match v with
        | T.SText2 pc  -> pc
        | _ -> failwith (spf "entry not TEXT: %s" entry)
        )
      (* normally impossible if propagated correctly, see main.ml *)
      with Not_found ->
        (* less: 5l does instead default to INITTEXT *)
        failwith (spf "entry not found: %s" entry)
      ;
  }
  in
  Logs.info (fun m -> m "saving executable in %s" (Chan.destination chan));
  (* Header *)
  A_out.write_header header chan.oc;

  (* Text section *)
  cs |> List.iter (Endian.Little.output_32 chan.oc);
  (* Data section *)
  (* no seek to a page boundary; a disk image is not a memory image! *)
  ds |> Array.iter (output_char chan.oc);
  (* todo: symbol table, program counter line table *)
  ()
