open Common

type symbol = string * scope
   and scope =
     | Public
     | Private of int (* represents a unique filename *)

(* increments by 1 *)
type virt_pc = int
(* increments by 4 for ARM *)
type real_pc = int

(* 32 bits *)
type addr = int
(* 32 bits *)
type word = int
(* 32 bits *)
type offset = int

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

(* after layout *)
type section2 =
  | SText2 of real_pc (* less: add auto_size information? *)
  (* offset to start of data section for ARM *)
  | SData2 of offset
  | SBss2 of offset

type value2 = {
  section2: section2;
}

type symbol_table2 = (symbol, value2) Hashtbl.t

type config = {
  header_type: string;
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

(* SXRef if not found *)
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
      else ()
  (* less: could report error when one define sig and not other *)
  | _ -> ()
  );
  v

  
