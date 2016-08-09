(*s: yacc/check.ml *)
(*s: copyright ocamlyacc *)
(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(*e: copyright ocamlyacc *)
open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO:
 * - exist 'start', and 'type' directives
 * - classic use/def: remember set of terms and non terms and look
 *   for use of undefined symbols, or unused symbols.
 * - wrong $ number, too big, $22 not handled for instance
 * - typechecking (but this is done for free by ocaml in the generated code)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type Check.error (yacc) *)
type error = unit
(*e: type Check.error (yacc) *)

(*s: exception Check.Error (yacc) *)
exception Error of error
(*e: exception Check.Error (yacc) *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function Check.report_error (yacc) *)
let report_error err =
  failwith "TODO"
(*e: function Check.report_error (yacc) *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
(*s: function Check.check (yacc) *)
let check def =
  failwith "TODO"
(*e: function Check.check (yacc) *)

(*e: yacc/check.ml *)
