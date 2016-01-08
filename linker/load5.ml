open Common

open Ast_asm5
module T = Types
module T5 = Types5

let error s =
  failwith s

(* modifies ent, modifies h *)
let process_ent ent h version =
  if true then raise Todo

(* visitors *)
let rec visit_ximm x =
  if true then raise Todo
    (* check_signature_ent e h !version *)

let rec visit_imm_or_ximm x =
  if true then raise Todo



(* load() performs a few things:
 * - split objects in code vs data,
 * - relocate branches,
 * - "name" entities by assigning a unique name to every entities
 *   (handling private symbols),
 * - visit all entities to add in symbol table. This is more complicated
 *   than in 5l because we can not rely on an ANAME or Operand.sym.
 *)
let load entry xs =

  (* values to return *)
  let code = ref [] in
  let data = ref [] in
  let h = Hashtbl.create 101 in

  let pc = ref 0 in
  let version = ref 0 in
  let curtext = ref "noname" in

  Hashtbl.add h (entry, T.Public) { T.section = T.SXref; signature = None; };

  xs |> List.iter (fun file ->
    let ipc = !pc in
    incr version;
    (* less: assert it is a .5 file *)

    (* object loading is so much easier in ocaml *)
    let (prog, _asmfile) = Object_code5.load file in

    prog |> List.iter (fun (p, line) ->
      match p with
      | Pseudo pseudo ->
          (match pseudo with
          | TEXT (ent, attrs, size) ->
              process_ent ent h !version;
              (* h |> Common.find_opt ent
                add symbol table, take care if private entity, use version
              *)
              code |> Common.push ({T5.
                instr = T5.TEXT (ent, attrs, size);
                virt_pc = !pc;
                line = line;
              });
              incr pc;
          | WORD v ->
              visit_imm_or_ximm v;
              code |> Common.push ({T5.
                instr = T5.WORD v;
                virt_pc = !pc;
                line = line;
              });
              incr pc;
            
          | GLOBL (ent, attrs, size) -> 
              process_ent ent h !version;
            raise Todo
          | DATA (ent, offset, size, v) -> 
              process_ent ent h !version;
              visit_imm_or_ximm v;
              data |> Common.push (T5.DATA (ent, offset, size, v))
                                    
          )
      (* todo: private symbol! *)
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
          code |> Common.push ({T5.
            instr = T5.I (inst, cond);
            virt_pc = !pc;
            line = line;
          });
          incr pc

      | LabelDef _ -> error (spf "label definition in object")
      | LineDirective _ ->
        (* todo: return also, but for now stripped info *)
        ()

    );
  );
  List.rev !code, List.rev !data, h
