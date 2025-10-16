(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external id : 'a -> 'a = "%identity"
let const c _ = c
let flip f x y = f y x
let negate p v = not (p v)


(* history: was called Utils.try_finally before, then it was 
 * exposed in pervasive.ml then moved to fun.ml
 *
 * Note that this old version was incorrect and could call finally (f2)
 * two times if finally itself was raising an exception
 *
 *     let try_finally f1 f2 =
 *       try
 *         let result = f1 () in
 *         f2 ();
 *         result
 *       with x -> f2 (); raise x
 *)
let protect ~finally work =
  let result = (try work () with e -> finally (); raise e) in
  finally ();
  result

(* TODO: last version is below 
   TODO: use Exception.ml and Fun_.ml like in semgrep?
exception Finally_raised of exn

let protect ~(finally : unit -> unit) work =
  let finally_no_exn () =
    try finally () with e ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace (Finally_raised e) bt
  in
  match work () with
  | result -> finally_no_exn () ; result
  | exception work_exn ->
      let work_bt = Printexc.get_raw_backtrace () in
      finally_no_exn () ;
      Printexc.raise_with_backtrace work_exn work_bt
*)
