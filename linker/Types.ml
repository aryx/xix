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

(* a single line number is not enough anymore, we need also the filename *)
(*s: type [[Types.loc]] *)
type loc = Fpath.t * A.loc
[@@deriving show]
(*e: type [[Types.loc]] *)

(* 8 bits *)
(*s: type [[Types.byte]] *)
type byte = char
[@@deriving show]
(*e: type [[Types.byte]] *)
(* 32 bits *)
(*s: type [[Types.word]] *)
type word = int
[@@deriving show]
(*e: type [[Types.word]] *)

(* 32 bits *)
(*s: type [[Types.addr]] *)
type addr = int
[@@deriving show]
(*e: type [[Types.addr]] *)
(* 32 bits *)
(*s: type [[Types.offset]] *)
type offset = int
[@@deriving show]
(*e: type [[Types.offset]] *)

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

(* increments by 1. Used as index in some 'code array' or 'node array' *)
(*s: type [[Types.virt_pc]] *)
type virt_pc = A.virt_pc
[@@deriving show]
(*e: type [[Types.virt_pc]] *)

(* before layout *)
(*s: type [[Types.section]] *)
type section =
  | SText of virt_pc
  | SData of int
  | SXref
[@@deriving show]
(*e: type [[Types.section]] *)

(* the filename is for safe linking error report *)
(*s: type [[Types.signature]] *)
type signature = int (* todo: * Common.filename *)
[@@deriving show]
(*e: type [[Types.signature]] *)

(*s: type [[Types.value]] *)
type value = {
  mutable section: section;
  sig_: signature option;
}
[@@deriving show]
(*e: type [[Types.value]] *)

(*s: type [[Types.symbol_table]] *)
type symbol_table = (symbol, value) Hashtbl.t
(*e: type [[Types.symbol_table]] *)
(*[@@deriving show]*)

(* --------------------------------------- *)
(* The real pc world *)
(* --------------------------------------- *)

(* increments by 4 for ARM *)
(*s: type [[Types.real_pc]] *)
type real_pc = int
[@@deriving show]
(*e: type [[Types.real_pc]] *)

(* after layout *)
(*s: type [[Types.section2]] *)
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
[@@deriving show]
(*e: type [[Types.value2]] *)

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
(* a subset of Ast_asm5.line (no GLOBL/DATA, no LabelDef/LineDirective) *)
(*s: type [[Types.code_bis]] *)
and 'instr code_bis =
  | TEXT of A.global * A.attributes * int
  | WORD of A.ximm
  | V of A.virtual_instr
  | I of 'instr
(*e: type [[Types.code_bis]] *)
[@@deriving show]

(* remember that GLOBL information is stored in symbol table  *)
(*s: type [[Types.data]] *)
type data = 
  | DATA of A.global * A.offset * int * A.ximm
[@@deriving show]
(*e: type [[Types.data]] *)


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
[@@deriving show]
(*e: type [[Types.node]] *)

(*s: type [[Types.code_graph]] *)
type 'instr code_graph = 'instr node (* the first node *)
[@@deriving show]
(*e: type [[Types.code_graph]] *)

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
