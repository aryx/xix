(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
module A = Ast_asm
module T = Types

(* for field access for ocaml-light *)
open Ast_asm
open Arch

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* "Names", modifies global, modifies h *)
let process_global (global : A.global) (h : T.symbol_table) (idfile : int) : unit =
  (match global.priv with
  | Some _ -> global.priv <- Some idfile
  | None -> ()
  );
  (* populate symbol table with SXref if new entity *)
  T.lookup_global global h |> ignore

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* load() performs a few things:
 * - load objects (of course),
 * - split and concatenate in code vs data all objects,
 * - relocate absolute jumps,
 * - "name" entities by assigning a unique name to every entities
 *   (handling private symbols),
 * - visit all entities (defs and uses) and add them in symbol table
 * alt: take as a parameter (xs : Chan.i list);
 *)
let load (caps : < Cap.open_in; ..>) (xs : Fpath.t list) (load_obj : Chan.i -> 'instr A.program * 'loc_history) (arch: 'instr Arch.t) : 'instr T.code array * T.data list * Types.symbol_table =

  (* values to return *)
  let code = ref [] in
  let data = ref [] in
  let h = Hashtbl.create 101 in

  let pc : Types.virt_pc ref = ref 0 in
  let idfile = ref 0 in

  xs |> List.iter (fun file ->
    let ipc : Types.virt_pc = !pc in
    incr idfile;
    (* less: assert it is a .5 file *)
    (* TODO: if lib file! *)

    (* object loading is so much easier in ocaml :) *)
    let (prog, _srcfile) = file |> FS.with_open_in caps load_obj in
    (* less: could check valid AST, range of registers, shift values, etc *)

    (* naming and populating symbol table h *)
    prog |> A.visit_globals_program arch.visit_globals_instr 
        (fun x -> process_global x h !idfile);

    (* split and concatenate in code vs data, relocate branches, 
     * and add definitions in symbol table h.
     *)
    prog |> List.iter (fun (p, line) ->
      match p with
      | A.Pseudo pseudo ->
          (match pseudo with
          | A.TEXT (global, attrs, size) ->
              (* less: set curtext for better error managment *)
              let v = T.lookup_global global h in
              (match v.T.section with
              | T.SXref -> v.T.section <- T.SText !pc;
              | _ -> failwith (spf "redefinition of %s" global.name)
              );
              (* less: adjust autosize? *)
              code |> Stack_.push (T.TEXT (global, attrs, size), (file, line));
              incr pc;
          | A.WORD v ->
              code |> Stack_.push (T.WORD v, (file, line));
              incr pc;
            
          | A.GLOBL (global, _attrs, size) -> 
              let v = T.lookup_global global h in
              (match v.T.section with
              | T.SXref -> v.T.section <- T.SData size;
              | _ -> failwith (spf "redefinition of %s" global.name)
              );
          | A.DATA (global, offset, size, v) -> 
              data |> Stack_.push (T.DATA (global, offset, size, v))
          )

      | A.Instr instr ->

          let relocate_branch opd =
            match !opd with
            | SymbolJump _ | IndirectJump _ -> ()
            | Relative _ | LabelUse _ ->
                raise (Impossible "Relative or LabelUse resolved by assembler")
            | Absolute i -> opd := Absolute (i + ipc)
          in
          arch.branch_opd_of_instr instr |> Option.iter relocate_branch;
          code |> Stack_.push (T.I instr, (file, line));
          incr pc;

      | A.LabelDef _ -> failwith (spf "label definition in object")
    );
  );
  Array.of_list (List.rev !code), List.rev !data, h
