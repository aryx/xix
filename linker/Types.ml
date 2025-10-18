(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* 8 bits *)
type byte = char
(* 32 bits *)
type word = int

(* 32 bits *)
type addr = int
(* 32 bits *)
type offset = int

type symbol = string * scope
   and scope =
     | Public
     | Private of int (* represents a unique filename, less: use filename? *)

(* --------------------------------------- *)
(* The virtual pc world *)
(* --------------------------------------- *)

(* increments by 1. Used as index in some 'code array' or 'node array' *)
type virt_pc = int

type section =
  | SText of virt_pc
  | SData of int
  | SXref

(* before layout *)
type value = {
  mutable section: section;
  (* the filename is for safe linking error report *)
  signature: (int (* todo: * Common.filename *)) option;
}

type symbol_table = (symbol, value) Hashtbl.t

(* --------------------------------------- *)
(* The real pc world *)
(* --------------------------------------- *)

(* increments by 4 for ARM *)
type real_pc = int

(* after layout *)
type section2 =
  | SText2 of real_pc
  (* offset to start of data section for ARM *)
  | SData2 of offset * data_kind
  and data_kind = Data | Bss

type value2 = section2

type symbol_table2 = (symbol, value2) Hashtbl.t

(* --------------------------------------- *)
(* The executable world *)
(* --------------------------------------- *)

type header_type =
  | A_out (* Plan9 *)
  | Elf (* Linux *)

type config = {
  header_type: header_type;
  header_size: int;
  init_text: addr;
  init_round: int;
  init_data: addr option;
  (* less: could be (string, addr) Common.either too *)
  entry_point: string;
}

type sections_size = {
  text_size: int;
  data_size: int;
  bss_size: int;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* create new entry with SXRef if not found *)
let lookup k sigopt h =
  let v =
    try
      Hashtbl.find h k
    with Not_found ->
      let v = { section = SXref; signature = sigopt } in
      Hashtbl.add h k v;
      v
  in
  (match sigopt, v.signature with
  | None, None -> ()
  | Some i1, Some i2 ->
      (* todo: report also offending object files *)
      if i1 <> i2
      then failwith (spf "incompatible type signatures %d and %d" i1 i2)
  (* less: could report error when one define sig and not other *)
  | _ -> ()
  );
  v

let s_of_symbol (s, scope) =
  (* less: could print the object filename instead *)
  s ^ (match scope with Public -> "" | Private _ -> "<>")
