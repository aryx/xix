(* 
 * Yoann Padioleau 
 *
 * Copyright (C) 2009-2012 Facebook
 * 
 * Most of the code in this file was inspired by code by Gazagnaire.
 * Here is the original copyright:
 * 
 * Copyright (c) 2009 Thomas Gazagnaire <thomas@gazagnaire.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Stdcompat (* for |> *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* 
 * OCaml hacks to support reflection.
 * 
 * OCaml does not support reflection, and it's a good thing: we love
 * strong type-checking that forbids too clever hacks like 'eval', or
 * run-time reflection; it's too much power for you, you will misuse
 * it. At the same time it's sometimes useful. So at least we could make
 * it possible to still reflect on the type definitions or values in
 * OCaml source code. We can do it by processing ML source code and
 * emitting ML source code containing under the form of regular ML
 * value or functions meta-information about information in other
 * source code files. It's a little bit a poor's man reflection mechanism,
 * because it's more manual, but it's for the best. Metaprogramming had
 * to be painful, because it is dangerous!
 * 
 * Example: 
 *  
 *      TODO
 * 
 * In some sense we reimplement what is in the OCaml compiler, which
 * contains the full AST of OCaml source code. But the OCaml compiler
 * and its AST are too big, too scary for many tasks that would be satisfied
 * by a restricted but simpler AST.
 * 
 * Camlp4 is obviously also a solution to this problem, but it has a
 * learning curve, and it's a slightly different world than the pure
 * regular OCaml world. So this module, and ocamltarzan together can
 * reduce the problem by taking the best of camlp4, while still
 * avoiding it.
 * 
 * 
 * 
 * The support is partial. We support only the OCaml constructions
 * we found the most useful for programming stuff like
 * stub generators. 
 * 
 * less? not all OCaml so call it miniml.ml  ? or reflection.ml ?
 * 
 * 
 * Notes: 2 worlds 
 *   - the type level world,
 *   - the data level world
 * 
 * Then there is whether the code is generated on the fly, or output somewhere
 * to be compiled and linked again (so 2 steps process, more manual, but
 * arguably less complicated magic)
 * 
 * different level of (meta)programming:
 *
 *  - programming in OCaml on OCaml values (classic)
 *  - programming in OCaml on Sexp.t value of value
 *  - programming in OCaml on Sexp.t value of type description
 *  - programming in OCaml on OCaml.v value of value
 *  - programming in OCaml on OCaml.t value of type description
 * 
 * Depending on what you have to do, some levels are more suited than other.
 * For instance to do a show, to pretty print value, then sexp is good,
 * because really you just want to write code that handle 2 cases, 
 * atoms and list. That's really what pretty printing is all about. You
 * could write a pretty printer for Ocaml.v, but it will need to handle
 * 10 cases. Now if you want to write a code generator for python, or an ORM,
 * then Ocaml.v is better than sexp, because in sexp you lost some valuable
 * information (that you may have to reverse engineer, like whether 
 * a Sexp.List corresponds to a field, or a sum, or wether something is
 * null or an empty list, or wether it's an int or float, etc).
 * 
 * Another way to do (meta)programming is:
 *  - programming in Camlp4 on OCaml ast
 *  - writing camlmix code to generate code.
 * 
 * notes:
 *  - sexp value or sexp of type description, not as precise, but easier to 
 *    write really generic code that do not need to have more information
 *    about the sexp nodes (such as wether it's a field, a constuctor, etc)
 *  - miniml value or type, not as precise that the regular type,
 *    but more precise than sexp, and allow write some generic code.
 *  - ocaml value (not type as you cant program at type level),
 *    precise type checking, but can be tedious to write generic
 *    code like generic visitors or pickler/unpicklers
 * 
 * This file is working with ocamltarzan/pa/pa_type.ml (and so indirectly
 * it is working with camlp4).
 * 
 * Note that can even generate sexp_of_x for miniML :) really
 * reflexive tower here
 * 
 * Note that even if this module helps a programmer to avoid
 * using directly camlp4 to auto generate some code, it can 
 * not solve all the tasks. 
 *
 * history: 
 *  - Thought about it when wanting to do the ast_php.ml to be
 *    transformed into a .adsl declaration to be able to generate
 *    corresponding python classes using astgen.py.
 *  - Thought about a miniMLType and miniMLValue, and then realize
 *    that that was maybe what code in the ocaml-orm-sqlite
 *    was doing (type-of et value-of), except I wanted the 
 *    ocamltarzan style of meta-programming instead of the camlp4 one.
 * 
 * 
 * Alternatives:
 *  - camlp4
 *    obviously camlp4 has access to the full AST of OCaml, but 
 *    that is one pb, that's too much. We often want only to do 
 *    analysis on the type
 *  - type-conv
 *    good, but force to use camlp4. Can use the generic sexplib
 *    and then work on the generated sexp, but as explained below, 
 *    is will be on the value.
 *  - use lib-sexp (just the sexp library part, not the camlp4 support part)
 *    but not enough info. Even if usually
 *    can reverse engineer the sexp to rediscover the type,
 *    you will reverse engineer a value; what you want
 *    is the sexp representation of the type! not a value of this type.
 *    Also lib-sexp autogenerated code can be hard to understand, especially
 *    if the type definition is complex. A good side effect of ocaml.ml
 *    is that it provides an intermediate step :) So even if you 
 *    could pretty print value from your def to sexp directly, you could
 *    also use transform your value into a Ocaml.v, then use 
 *    the somehow more readable function that translate a v into a sexp,
 *    and same when wanting to read a value from a sexp, by using
 *    again Ocaml.v as an intermediate. It's nevertheless obviously
 *    less efficient.
 * 
 *  - zephyr, or thrift ?
 *  - F# ?
 *  - Lisp/Scheme ?
 *  - .Net interoperability
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* src: 
 *  - orm-sqlite/value/value.ml
 *  (itself a fork of http://xenbits.xen.org/xapi/xen-api-libs.hg?file/7a17b2ab5cfc/rpc-light/rpc.ml)
 *  - orm-sqlite/type-of/type.ml
 * 
 * update: Gazagnaire made a paper about that.
 * 
 * modifications: 
 *  - slightly renamed the types and rearrange order of constructors.  Could 
 *    have use nested modules to allow to reuse Int in different contexts,  
 *    but I actually prefer to prefix the values with the V, so when debugging
 *    stuff, it's clearer that what you are looking are values, not types
 *    (even if the ocaml toplevel would prefix the value with a V. or T.,
 *    but sexp would not)
 *  - Changed Int of int option
 *  - Introduced List, Apply, Poly
 *  - debugging support (using sexp :) )
 *)

(* OCaml values (a restricted form of expressions) *)
type v = 
  | VUnit 
  | VBool of bool | VFloat of float | VInt of int (* was int64 *)
  | VChar of char | VString of string

  | VTuple of v list
  | VDict of (string * v) list
  | VSum of string * v list

  | VVar of (string * (* was int64 *) int)
  | VArrow of string

  (* special cases *) 
  | VNone | VSome of v
  | VList of v list
  | VRef of v

(*
  | VEnum of v list (* ??? *)
  | VRec of (string * int64) * v
  | VExt of (string * int64) * v
*)
  | VTODO of string

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* for generated code that want to transform and in and out of a v or t *)
let vof_unit () = 
  VUnit
let vof_int x = 
  VInt ((*Int64.of_int*) x)
let vof_float x = 
  VFloat ((*Int64.of_int*) x)
let vof_string x = 
  VString x
let vof_bool b = 
  VBool b
let vof_list ofa x = 
  VList (List.map ofa x)
let vof_option ofa x =
  match x with
  | None -> VNone
  | Some x -> VSome (ofa x)
let vof_ref ofa x =
  match x with
  | {contents = x } -> VRef (ofa x)
let vof_either _of_a _of_b =
  function
  | Either.Left v1 -> let v1 = _of_a v1 in VSum (("Left", [ v1 ]))
  | Either.Right v1 -> let v1 = _of_b v1 in VSum (("Right", [ v1 ]))

(*****************************************************************************)
(* Format pretty printers *)
(*****************************************************************************)

(* TODO: use Buffer_.with_buffer_to_string and Fmt_.with_buffer_to_string *)
(* Used by OCaml.ml *)
(* julia: convert something printed using format to print into a string *)
let format_to_string f =
  (* ocaml-light: let (nm,o) = Filename.open_temp_file "format_to_s" ".out" in *)
  let nm = Filename.temp_file "format_to_s" ".out" in
  let o = open_out nm in
  (* to avoid interference with other code using Format.printf, e.g.
   * Ounit.run_tt
   *)
  Format.print_flush();
  Format.set_formatter_out_channel o;
  let _ = f () in
  Format.print_newline();
  Format.print_flush();
  Format.set_formatter_out_channel stdout;
  close_out o;
  let i = open_in nm in
  let lines = ref [] in
  let rec loop _ =
    let cur = input_line i in
    lines := cur :: !lines;
    loop() in
  (try loop() with End_of_file -> ());
  close_in i;
  Sys.command ("rm -f " ^ nm) |> ignore;
  String.concat "\n" (List.rev !lines)

let add_sep xs = 
  xs |> List.map (fun x -> Either.Right x) |> Common2.join_gen (Either.Left ())

(* 
 * OCaml value pretty printer. A similar functionnality is provided by
 * the OCaml toplevel interpreter ('/usr/bin/ocaml') but 
 * sometimes it is useful to print values from a regular command 
 * line program. You don't always want to run the ocaml interpreter (or 
 * customized interpreter built by ocamlmktop), and type an expression
 * in to get the printed value.
 * 
 * The v_of_xxx generated code by ocamltarzan is 
 * the first part to make this possible. The function below
 * is the second part.
 * 
 * The '@[', '@,', etc are Format printf tags. See the doc of the Format
 * module in the OCaml manual to understand their meaning. Mainly, 
 * @[ and @] open and close a pretty print box, and '@ ' and '@,' 
 * are to give breaking hints to the pretty printer.
 * 
 * The output can be copy pasted in ML code directly, which can be 
 * useful when you want to pattern match over complex ocaml value.
 *)

let string_of_v v = 
  format_to_string (fun () ->
    let ppf = Format.printf in
    let rec aux v = 
      match v with
      | VUnit -> ppf "()"
      | VBool v1 ->
          if v1
          then ppf "true"
          else ppf "false"
      | VFloat v1 -> ppf "%f" v1
      | VChar v1 -> ppf "'%c'" v1
      | VString v1 -> ppf "\"%s\"" v1
      | VInt i -> ppf "%d" i
      | VTuple xs ->
          ppf "(@[";
              xs |> add_sep |> List.iter (function
              | Either.Left _ -> ppf ",@ ";
              | Either.Right v -> aux v
              );
          ppf "@])";
      | VDict xs ->
          ppf "{@[";
          xs |> List.iter (fun (s, v) ->
            (* less: could open a box there too? *)
            ppf "@,%s=" s;
            aux v;
            ppf ";@ ";
          );
          ppf "@]}";
          
      | VSum ((s, xs)) ->
          (match xs with
          | [] -> ppf "%s" s
          | _y::_ys ->
              ppf "@[<hov 2>%s(@," s;
              xs |> add_sep |> List.iter (function
              | Either.Left _ -> ppf ",@ ";
              | Either.Right v -> aux v
              );
              ppf "@])";
          )
          
      | VVar (s, x) -> ppf "%s_%d" s ((*Int64.to_int*) x)
      | VArrow _v1 -> failwith "Arrow TODO"
      | VNone -> ppf "None";
      | VSome v -> ppf "Some(@["; aux v; ppf "@])";
      | VRef v -> ppf "Ref(@["; aux v; ppf "@])";
      | VList xs ->
          ppf "[@[<hov>";
          xs |> add_sep |> List.iter (function
          | Either.Left _ -> ppf ";@ ";
          | Either.Right v -> aux v
          );
          ppf "@]]";
      | VTODO _v1 -> ppf "VTODO"
    in
    aux v
  )
