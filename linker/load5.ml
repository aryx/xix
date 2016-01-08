open Common

open Ast_asm5
module T = Types
module T5 = Types5

let error s =
  failwith s

let lookup_ent ent h =
  let symbol = T5.symbol_of_entity ent in
  T.lookup symbol ent.signature h

(* modifies ent, modifies h *)
let process_ent ent h idfile =
  (match ent.priv with
  | Some _ -> ent.priv <- Some idfile
  | None -> ()
  );
  lookup_ent ent h |> ignore

(* visit entities to populate symbol table with wanted symbols 
 * This is more complicated than in 5l because we can not rely
 * on an ANAME or Operand.sym.
 * boilerplate, could be auto generated.
 *)

let visit_entities_line f x =
  let rec ximm x =
    if true then raise Todo
    (* process_ent e h idfile *)
  in
  let rec imm_or_ximm x =
    if true then raise Todo
  in
  let rec line x =
    raise Todo
  in
  match x with
  | Pseudo 
  raise Todo



(* load() performs a few things:
 * - load objects (of course),
 * - split and concatenate in code vs data all objects,
 * - relocate absolute jumps,
 * - "name" entities by assigning a unique name to every entities
 *   (handling private symbols),
 * - visit all entities (defs and uses) and add them in symbol table
 *)
let load entry xs =

  (* values to return *)
  let code = ref [] in
  let data = ref [] in
  let h = Hashtbl.create 101 in

  let pc = ref 0 in
  let idfile = ref 0 in

  (* =~ T.lookup (entry, T.Public) None h |> ignore *)
  Hashtbl.add h (entry, T.Public) { T.section = T.SXref; signature = None; };

  xs |> List.iter (fun file ->
    let ipc = !pc in
    incr idfile;
    (* less: assert it is a .5 file *)

    (* object loading is so much easier in ocaml *)
    let (prog, _srcfile) = Object_code5.load file in

    prog |> List.iter (fun (p, line) ->
      visit_entities_line (fun ent ->
        process_ent ent h !idfile;
      );
      match p with
      | Pseudo pseudo ->
          (match pseudo with
          | TEXT (ent, attrs, size) ->
              (* less: set curtext *)
              let v = lookup_ent ent h in
              (match v.T.section with
              | T.SXref -> v.T.section <- T.SText !pc;
              | _ -> error (spf "redefinition of %s" ent.name)
              );
              (* less: adjust autosize? *)
              code |> Common.push (T5.TEXT (ent, attrs, size), line);
              incr pc;
          | WORD v ->
              code |> Common.push (T5.WORD v, line);
              incr pc;
            
          | GLOBL (ent, attrs, size) -> 
              let v = lookup_ent ent h in
              (match v.T.section with
              | T.SXref -> v.T.section <- T.SBss size;
              | _ -> error (spf "redefinition of %s" ent.name)
              );
          | DATA (ent, offset, size, v) -> 
              data |> Common.push (T5.DATA (ent, offset, size, v))
          )

      | Instr (inst, cond) ->
          let relocate_branch opd =
            match opd with
            | SymbolJump _ | IndirectJump _ -> opd
            | Relative _ | LabelUse _ ->
                error "use of label or relative jump in object"
            | Absolute i -> Absolute (i + ipc)
          in
          let inst = 
            match inst with
            | B opd -> B (relocate_branch opd)
            | BL opd -> BL (relocate_branch opd)
            | Bxx (cond, opd) -> Bxx (cond, relocate_branch opd)
            | _ -> inst
          in
          code |> Common.push (T5.I (inst, cond), line);
          incr pc;

      | LabelDef _ -> error (spf "label definition in object")
      (* todo: return also, but for now stripped info *)
      | LineDirective _ -> ()
    );
  );
  Array.of_list (List.rev !code), List.rev !data, h
