(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast_asm5

(* see how little processing 5a actually does *)
let resolve ps =
  let pc = ref 0 in
  let h = Hashtbl.create 101 in

  (* first pass, process the label definitions *)
  ps |> List.iter (fun (p, line) ->
    (match p with
    | LabelDef lbl -> 
        (* better check duplicate here than via lexing tricks *)
        if Hashtbl.mem h lbl
        then failwith (spf "redeclaration of %s at line %d" lbl line);
        Hashtbl.add h lbl !pc;
        (* no increment, share pc if multiple labels at same place *)
    | Pseudo _ | Instr _ -> incr pc
    | LineDirective _ -> ()
    )
  );

  (* second pass, process the label uses *)
  pc := 0;
  ps |> Common.map_filter (fun (p, line) ->
    (match p with
    (* no need keep labels in object files *)
    | LabelDef _ -> None
    | Pseudo _ -> incr pc; Some (p, line)
    | LineDirective _ -> Some (p, line)
    | Instr (inst, cond) ->
      let inst = 
        match inst with
        | B opd | BL opd | Bxx (_, opd)  ->
            let opd =
              match opd with
              | SymbolJump _ | IndirectJump _ -> opd
              | Relative i -> Absolute (!pc + i)
              | LabelUse (lbl, i) ->
                  (try
                     let pc = Hashtbl.find h lbl in
                     Absolute (pc + i)
                   with Not_found ->
                     failwith (spf "undefined label: %s, at line %d" lbl line)
                  )
              | Absolute _ -> raise Impossible
            in
            (match inst with
            | B _ -> B opd
            | BL _ -> BL opd
            | Bxx (cond, _) -> Bxx (cond, opd)
            | _ -> raise Impossible
            )
        | _ -> inst
      in
      incr pc;
      Some (Instr (inst, cond), line)
    )
  )
