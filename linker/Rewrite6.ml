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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* claude: a real no-op for this first checkpoint, unlike every other
 * arch's own Rewrite*.ml -- amd64 needs neither of the two things
 * those exist for:
 *  - No auto-generated prologue/epilogue: ARM32/MIPS/RISC-V/ARM64 all
 *    synthesize a link-register save/restore sequence around a TEXT's
 *    body based on its declared frame size (see e.g. Rewrite5.ml's own
 *    step2). amd64's CALL/RET push/pop the return address in hardware,
 *    and any real stack adjustment (goken's hello_linux_amd64.s's own
 *    "SUBQ $16,SP"/"ADDQ $16,SP") is already explicit, ordinary
 *    instructions straight from the source -- nothing to synthesize.
 *  - No virtual-instruction expansion: RET is a real, directly-
 *    encodable hardware instruction here (Ast_asm6.ml's `Ret`, exactly
 *    like ARM64's own real RET), not a compiler-facing pseudo-op
 *    needing a Rewrite pass to turn into a real branch the way
 *    ARM32/MIPS/RISC-V's `Ast_asm.virtual_instr.RET` does.
 *
 * FP-relative addressing ("buf+0(FP)") IS resolved using the enclosing
 * TEXT's frame size, same idea as every other arch -- but that
 * resolution happens directly in Codegen6.ml's `resolve_gen` (reading
 * `env.autosize`, threaded from Layout6.ml/Codegen6.gen's own TEXT
 * tracking), not here, since no AST rewriting is needed to make it
 * possible (see Codegen6.ml's own comment on `Entity (A.Local ...)`
 * for the exact offset formula, "+8" for amd64's own hardware-pushed
 * return address). *)
let rewrite (cg : 'a Types.code_graph) : 'a Types.code_graph = cg
