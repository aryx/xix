open Common

module T = Types
module T5 = Types5

let lput chan word =
  if true then raise Todo

let gen config sizes cs ds symbols outfile =
 outfile |> Common.with_file_out (fun chan ->

  if config.T.header_type <> "a.out"
  then failwith (spf "executable format not supported: %s" 
                   config.T.header_type);

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
        let v = Hashtbl.find symbols (config.T.entry_point, T.Public) in
        (match v.T.section2 with
        | T.SText2 pc  -> pc
        | _ -> failwith (spf "entry not TEXT: %s" config.T.entry_point)
        )
      with Not_found ->
        (* less: 5l does instead default to INITTEXT *)
        failwith (spf "entry not found: %s" config.T.entry_point)
      ;
  }
  in
  (* generate header *)
  A_out.write_header header chan;

  cs |> List.iter (lput chan);
  (* no seek to a page boundary; a disk image is not a memory image *)
  ds |> List.iter (lput chan);
  
  (* todo: symbol table, program counter line table *)
 )
