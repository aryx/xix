(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

module A = Ast_asm

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* --------------------------------------- *)
(* basic types *)
(* --------------------------------------- *)

(* a single line number is not enough anymore, we need also the filename *)
type loc = Fpath.t * A.loc
[@@deriving show]

(* 8 bits *)
type byte = char
[@@deriving show]
(* 32 bits *)
type word = int
[@@deriving show]

(* 32 bits *)
type addr = int
[@@deriving show]
(* 32 bits *)
type offset = int
[@@deriving show]

type symbol = string * scope
   and scope =
     | Public
     | Private of int (* represents a unique filename, less: use filename? *)
[@@deriving show]

(* --------------------------------------- *)
(* The virtual pc world *)
(* --------------------------------------- *)

(* increments by 1. Used as index in some 'code array' or 'node array' *)
type virt_pc = A.virt_pc
[@@deriving show]

(* before layout *)
type section =
  | SText of virt_pc
  | SData of int
  | SXref
[@@deriving show]

(* the filename is for safe linking error report *)
type signature = int (* todo: * Common.filename *)
[@@deriving show]

type value = {
  mutable section: section;
  sig_: signature option;
}
[@@deriving show]

type symbol_table = (symbol, value) Hashtbl.t
(*[@@deriving show]*)

(* --------------------------------------- *)
(* The real pc world *)
(* --------------------------------------- *)

(* increments by 4 for ARM *)
type real_pc = int
[@@deriving show]

(* after layout *)
type section2 =
  | SText2 of real_pc
  (* offset to start of data section for ARM *)
  | SData2 of offset * data_kind
  and data_kind = Data | Bss
[@@deriving show]

type value2 = section2
[@@deriving show]

type symbol_table2 = (symbol, value2) Hashtbl.t
(*[@@deriving show]*)

(* --------------------------------------- *)
(* Code vs Data *)
(* --------------------------------------- *)


(* Split Asm instructions in code vs data.
 *
 * For 'code' below we want to do some naming. We could copy many of 
 * ast_asm.ml and replace 'global' with the fully resolved 'symbol'.
 * But it would be a big copy paste. Instead, we opted for a mutable field 
 * in ast_asm.ml set by the linker (see Ast_asm.entity.priv).
 *)
type 'instr code = ('instr code_bis * loc)
(* a subset of Ast_asm5.line (no GLOBL/DATA, no LabelDef/LineDirective) *)
and 'instr code_bis =
  | TEXT of A.global * A.attributes * int
  | WORD of A.ximm
  | V of A.virtual_instr
  | I of 'instr
[@@deriving show]

(* remember that GLOBL information is stored in symbol table  *)
type data = 
  | DATA of A.global * A.offset * int * A.ximm
[@@deriving show]


(* graph via pointers, like in original 5l *)
type 'instr node = {
  (* can be altered during rewriting *)
  mutable instr: 'instr code_bis;
  mutable next: 'instr node option;
  (* for branching instructions and also for instructions using the pool *)
  mutable branch: 'instr node option;
  
  (* set after layout_text (set to -1 initially) *)
  mutable real_pc: real_pc;

  n_loc: loc;
}
[@@deriving show]

type 'instr code_graph = 'instr node (* the first node *)
[@@deriving show]


(* --------------------------------------- *)
(* The executable world *)
(* --------------------------------------- *)

type header_type =
  | A_out (* Plan9 *)
  | Elf (* Linux *)
[@@deriving show]

(* for ocaml-light to work without deriving *)
let show_config _ = "NO DERIVING"
[@@warning "-32"]

type config = {
  header_type: header_type;
  header_size: int;
  init_text: addr;
  init_round: int;
  init_data: addr option;
  (* less: could be (string, addr) Common.either too *)
  entry_point: string;
}
[@@deriving show]

type sections_size = {
  text_size: int;
  data_size: int;
  bss_size: int;
}
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* create new entry with SXRef if not found *)
let lookup (k : symbol) (sigopt : signature option) (h : symbol_table) : value =
  let v =
    try
      Hashtbl.find h k
    with Not_found ->
      let v = { section = SXref; sig_ = sigopt } in
      Hashtbl.add h k v;
      v
  in
  (match sigopt, v.sig_ with
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

(* assert not Some -1 ! should have been set during loading! *)
let symbol_of_global (e : A.global) : symbol =
  e.name, (match e.priv with None -> Public | Some i -> Private i)

let lookup_global (x : A.global) (h : symbol_table) : value =
  let symbol = symbol_of_global x in
  lookup symbol x.signature h


(* less: would need Hist mapping for this file to convert to original source *)
let s_of_loc (file, line) =
  spf "%s:%d" !!file line

let rec iter f n =
  f n;
  n.next |> Option.iter (fun n -> iter f n)

let rec iter_with_env f env n =
  let env = f env n in
  n.next |> Option.iter (fun n -> iter_with_env f env n)
