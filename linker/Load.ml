(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
module A = Ast_asm
module T = Types

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* "Names", modifies global, modifies h *)
let process_global (global : A.global) (h : T.symbol_table) (idfile : int) : unit =
  (match global.priv with
  | Some _ -> global.A.priv <- Some idfile
  | None -> ()
  );
  (* populate symbol table with SXref if new entity *)
  T.lookup_global global h |> ignore

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* load() performs a few things:
 * - load objects (of course), and libraries (which are essentially objects)
 * - split and concatenate in code vs data all objects,
 * - relocate absolute jumps,
 * - "name" entities by assigning a unique name to every entities
 *   (handling private symbols),
 * - visit all entities (defs and uses) and add them in symbol table
 *
 * alt: take as a parameter (xs : Chan.i list);
 *)
let load (caps : < Cap.open_in; ..>) (xs : Fpath.t list) (arch: 'instr Arch_linker.t) : 'instr T.code array * T.data list * Types.symbol_table =

  (* values to return *)
  let code = ref [] in
  let data = ref [] in
  let h = Hashtbl.create 101 in

  let pc : Types.virt_pc ref = ref 0 in
  let idfile = ref 0 in

  let process_obj (obj : 'instr Object_file.t) =
    let file = Fpath.v "TODO" in
    let prog = obj.prog in
    let ipc : Types.virt_pc = !pc in
    incr idfile;
    

    (* naming and populating symbol table h *)
    prog |> A.visit_globals_program arch.visit_globals_instr 
        (fun x -> process_global x h !idfile);

    let (ps, _locs) = prog in
    (* split and concatenate in code vs data, relocate branches, 
     * and add definitions in symbol table h.
     *)
    ps |> List.iter (fun (p, line) ->
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

      | A.Virtual instr ->
          code |> Stack_.push (T.V instr, (file, line));
          incr pc;
      | A.Instr instr ->

          let relocate_branch opd =
            match !opd with
            | A.SymbolJump _ | A.IndirectJump _ -> ()
            | A.Relative _ | A.LabelUse _ ->
                raise (Impossible "Relative or LabelUse resolved by assembler")
            | A.Absolute i -> opd := A.Absolute (i + ipc)
          in
          arch.branch_opd_of_instr instr |> Option.iter relocate_branch;
          code |> Stack_.push (T.I instr, (file, line));
          incr pc;

      | A.LabelDef _ -> failwith (spf "label definition in object")
    );
  in

  (* TODO: split in obj file vs libfile and process libfile at the end
   * and leverage SYMDEF/ranlib index to optimize
  *)
  xs |> List.iter (fun file ->
    match () with
    | _ when Library_file.is_lib_filename file ->
         let (objs : 'instr Library_file.t)  = 
            file |> FS.with_open_in caps Library_file.load in
         (* TODO: filter only the one needed because contain entities
          * used in the object files
          *)
         objs |> List.iter (fun obj -> process_obj obj)
    | _ when Object_file.is_obj_filename file ->
      (* object loading is so much easier in ocaml :) *)
      let (obj : 'instr Object_file.t) =
            file |> FS.with_open_in caps Object_file.load in
      process_obj obj
     (* less: could check valid AST, range of registers, shift values, etc *)

    | _ -> failwith (spf "file %s does not appear to be an obj or lib file"
            !!file)
  );

  Array.of_list (List.rev !code), List.rev !data, h
