open Common

module T = Types
module T5 = Types5

let lput chan word =
  if true then raise Todo

let gen config sizes cs ds symbols outfile =
 outfile |> Common.with_file_out (fun chan ->

  let entry = config.T.entry_point in
  let format = config.T.header_type in

  if format  <> "a.out"
  then failwith (spf "executable format not supported: %s" format);

  let header = { A_out.
     magic = 0x647;
     text_size = sizes.T.text_size;
     data_size = sizes.T.data_size;
     bss_size = sizes.T.bss_size;

     (* todo: for now stripped *)
     symbol_size = 0;
     pc_size = 0;
     
     entry =
      try 
        let v = Hashtbl.find symbols (entry, T.Public) in
        (match v.T.section2 with
        | T.SText2 pc  -> pc
        | _ -> failwith (spf "entry not TEXT: %s" entry)
        )
      with Not_found ->
        (* less: 5l does instead default to INITTEXT *)
        failwith (spf "entry not found: %s" entry)
      ;
  }
  in
  (* Header *)
  A_out.write_header header chan;

  (* Text section *)
  cs |> List.iter (lput chan);
  (* Data section *)
  (* no seek to a page boundary; a disk image is not a memory image! *)
  ds |> List.iter (lput chan);
  
  (* todo: symbol table, program counter line table *)
 )
