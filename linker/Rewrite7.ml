(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, with the special
 * exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

module T = Types
open Ast_asm7

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* claude: unlike Rewrite5.ml/Rewritev.ml/Rewritei.ml, this is currently a
 * no-op pass-through, not a TEXT/RET-leaf-detection rewrite. Two reasons:
 *  - ARM64 has a *real* RET hardware instruction (see Ast_asm7.ml's RET
 *    comment) -- unlike 5a/va/ia, which have no such mnemonic at all, so
 *    goken's own noop.c/5l-vl-il-style RET synthesis from a frame size
 *    genuinely has no ARM64 equivalent to port here; a real .s fixture
 *    just writes "ADD $n,RSP,RSP" / "RET" directly.
 *  - Parser_asm7.mly doesn't parse the shared `virtual_instr` production
 *    at all yet (no grammar rule reduces to `Ast_asm.virtual_instr`), so
 *    RET/NOP/Load/Store/AddI/Jmp/JmpAndLink/Cmp/JEq (the
 *    compiler-facing virtual instrs a future occ ARM64 backend would
 *    emit) can never actually appear in a graph built from real .s text
 *    yet -- the `T.Virt _ -> raise (Impossible ...)` arm below is
 *    genuinely unreachable today, not a silently-wrong stub.
 * Revisit both points together if/when a real driver needs them (e.g. a
 * future occ ARM64 backend wanting TEXT-declared-frame-size prologues),
 * following Rewritei.ml's shape as the closest template (also no branch-
 * delay slots to worry about, so likely simpler than Rewritev.ml/
 * Rewrite5.ml). *)

let rewrite (cg : instr T.code_graph) : instr T.code_graph =
  cg |> T.iter (fun (n : instr T.node) ->
    match n.T.instr with
    | T.TEXT _ | T.WORD _ -> ()
    | T.Virt _ ->
        raise (Impossible
          "Parser_asm7.mly does not produce virtual_instr nodes yet")
    | T.I _ -> ()
  );
  cg
