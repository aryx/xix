(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

module T = Types
module T5 = Types5

(* little-endian put long (see also lput in a_out.ml for big-endian version) *)
let lputl chan word =
  if word < 0 
  then raise (Impossible (spf "should call lputl with a uint not %d" word));

  let x1 = Char.chr (word mod 256) in
  let x2 = Char.chr ((word lsr 8) mod 256) in
  let x3 = Char.chr ((word lsr 16) mod 256) in
  let x4 = Char.chr ((word lsr 24) mod 256) in
  output_char chan x1;
  output_char chan x2;
  output_char chan x3;
  output_char chan x4;
  ()
  

let cput chan byte =
  output_char chan byte
  

let gen config sizes cs ds symbols2 outfile =
 let outfile = Fpath.v outfile in
 outfile |> UChan.with_open_out (fun (chan : Chan.o) ->

  let entry = config.T.entry_point in
  let format = config.T.header_type in

  if format  <> "a.out"
  then failwith (spf "executable format not supported: %s" format);

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
  (* Header *)
  A_out.write_header header chan.oc;

  (* Text section *)
  cs |> List.iter (lputl chan.oc);
  (* Data section *)
  (* no seek to a page boundary; a disk image is not a memory image! *)
  ds |> Array.iter (cput chan.oc);
  
  (* todo: symbol table, program counter line table *)
 )
