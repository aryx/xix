(*s: Flags.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*s: constant [[Flags.warn]] *)
let warn = ref false
(*e: constant [[Flags.warn]] *)
(*s: constant [[Flags.warnerror]] *)
let warnerror = ref false
(*e: constant [[Flags.warnerror]] *)

(* see also macroprocessor/flags_cpp.ml *)

(*s: constant [[Flags.dump_tokens]] *)
let dump_tokens = ref false
(*e: constant [[Flags.dump_tokens]] *)
(*s: constant [[Flags.dump_ast]] *)
let dump_ast = ref false
(*e: constant [[Flags.dump_ast]] *)
(*s: constant [[Flags.dump_typed_ast]] *)
let dump_typed_ast = ref false
(*e: constant [[Flags.dump_typed_ast]] *)
(*s: constant [[Flags.dump_asm]] *)
let dump_asm = ref false
(*e: constant [[Flags.dump_asm]] *)

(*s: constant [[Flags.debugger]] *)
let debugger = ref false
(*e: constant [[Flags.debugger]] *)
(*s: constant [[Flags.backtrace]] *)
let backtrace = ref false
(*e: constant [[Flags.backtrace]] *)
(*e: Flags.ml *)
