(*s: Types.ml *)
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

(*s: type [[Types.loc]] *)
(* a single line number is not enough anymore, we need also the filename *)
type loc = Fpath.t * A.loc
(*e: type [[Types.loc]] *)
[@@deriving show]

(*s: type [[Types.byte]] *)
(* 8 bits *)
type byte = char
(*e: type [[Types.byte]] *)
[@@deriving show]
(*s: type [[Types.word]] *)
(* 32 bits *)
type word = int
(*e: type [[Types.word]] *)
[@@deriving show]

(*s: type [[Types.addr]] *)
(* 32 bits *)
type addr = int
(*e: type [[Types.addr]] *)
[@@deriving show]
(*s: type [[Types.offset]] *)
(* 32 bits *)
type offset = int
(*e: type [[Types.offset]] *)
[@@deriving show]

(*s: type [[Types.symbol]] *)
type symbol = string * scope
(*e: type [[Types.symbol]] *)
(*s: type [[Types.scope]] *)
   and scope =
     | Public
     | Private of int (* represents a unique filename, less: use filename? *)
(*e: type [[Types.scope]] *)
[@@deriving show]

(* --------------------------------------- *)
(* The virtual pc world *)
(* --------------------------------------- *)

(*s: type [[Types.virt_pc]] *)
(* increments by 1. Used as index in some 'code array' or 'node array' *)
type virt_pc = A.virt_pc
(*e: type [[Types.virt_pc]] *)
[@@deriving show]

(*s: type [[Types.section]] *)
(* before layout *)
type section =
  | SText of virt_pc
  | SData of int (* size *)
  | SXref (* undefined, alt: section option and use None *)
(*e: type [[Types.section]] *)
[@@deriving show]

(*s: type [[Types.signature]] *)
(* the filename is for safe linking error report *)
type signature = int (* todo: * Common.filename *)
(*e: type [[Types.signature]] *)
[@@deriving show]

(*s: type [[Types.value]] *)
type value = {
  mutable section: section;
  sig_: signature option;
}
(*e: type [[Types.value]] *)
[@@deriving show]

(*s: type [[Types.symbol_table]] *)
type symbol_table = (symbol, value) Hashtbl.t
(*e: type [[Types.symbol_table]] *)
(*[@@deriving show]*)

(* --------------------------------------- *)
(* The real pc world *)
(* --------------------------------------- *)

(*s: type [[Types.real_pc]] *)
(* increments by 4 for ARM *)
type real_pc = int
(*e: type [[Types.real_pc]] *)
[@@deriving show]

(*s: type [[Types.section2]] *)
(* after layout *)
type section2 =
  | SText2 of real_pc
  (* offset to start of data section for ARM *)
  | SData2 of offset * data_kind
(*e: type [[Types.section2]] *)
(*s: type [[Types.data_kind]] *)
  and data_kind = Data | Bss
(*e: type [[Types.data_kind]] *)
[@@deriving show]

(*s: type [[Types.value2]] *)
type value2 = section2
(*e: type [[Types.value2]] *)
[@@deriving show]

(*s: type [[Types.symbol_table2]] *)
type symbol_table2 = (symbol, value2) Hashtbl.t
(*e: type [[Types.symbol_table2]] *)
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
(*s: type [[Types.code]] *)
type 'instr code = ('instr code_bis * loc)
(*e: type [[Types.code]] *)
(*s: type [[Types.code_bis]] *)
(* a subset of Ast_asm5.line (no GLOBL/DATA, no LabelDef/LineDirective) *)
and 'instr code_bis =
  | TEXT of A.global * A.attributes * int
  | WORD of A.ximm
  | Virt of A.virtual_instr
  | I of 'instr
(*e: type [[Types.code_bis]] *)
[@@deriving show {with_path = false}]

(*s: type [[Types.data]] *)
(* remember that GLOBL information is stored in symbol table  *)
type data = 
  | DATA of A.global * A.offset * int * A.ximm
(*e: type [[Types.data]] *)
[@@deriving show]


(* graph via pointers, like in original 5l *)
(*s: type [[Types.node]] *)
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
(*e: type [[Types.node]] *)
[@@deriving show]

(*s: type [[Types.code_graph]] *)
type 'instr code_graph = 'instr node (* the first node *)
(*e: type [[Types.code_graph]] *)
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Types.lookup]] *)
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
(*e: function [[Types.lookup]] *)

(*s: function [[Types.s_of_symbol]] *)
let s_of_symbol (s, scope) =
  (* less: could print the object filename instead *)
  s ^ (match scope with Public -> "" | Private _ -> "<>")
(*e: function [[Types.s_of_symbol]] *)

(*s: function [[Types.symbol_of_global]] *)
(* assert not Some -1 ! should have been set during loading! *)
let symbol_of_global (e : A.global) : symbol =
  e.name, (match e.priv with None -> Public | Some i -> Private i)
(*e: function [[Types.symbol_of_global]] *)

(*s: function [[Types.lookup_global]] *)
let lookup_global (x : A.global) (h : symbol_table) : value =
  let symbol = symbol_of_global x in
  lookup symbol x.signature h
(*e: function [[Types.lookup_global]] *)


(*s: function [[Types.s_of_loc]] *)
(* less: would need Hist mapping for this file to convert to original source *)
let s_of_loc (file, line) =
  spf "%s:%d" !!file line
(*e: function [[Types.s_of_loc]] *)

(*s: function [[Types.iter]] *)
let rec iter f n =
  f n;
  n.next |> Option.iter (fun n -> iter f n)
(*e: function [[Types.iter]] *)

(*s: function [[Types.iter_with_env]] *)
let rec iter_with_env f env n =
  let env = f env n in
  n.next |> Option.iter (fun n -> iter_with_env f env n)
(*e: function [[Types.iter_with_env]] *)
(*e: Types.ml *)
