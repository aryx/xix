(* Yoann Padioleau
 *
 * Copyright (C) 2025 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Declarative bits building.
 *
 * alt:
 *  - use bit bool ops and bit shifting ops at build time, like in C,
 *    but more tedious in OCaml than C and anyway lose some check opportunity
 *  - use struct bitfields like in C? some bitfield libs for that in OCaml?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* More declarative way to build integers from bits and give opportunity to
 * sanity check if overlap.
 * TODO: (int * intsized) list; and intsized = I1 of int | I2 of int | I3 of int
 * to store number of bits used and extend sanity check to check for
 * overflow?
*)
type t = (int * int) list
[@@deriving show]

type int32 = t
type int64 = t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Can detect some typing mistakes *)
let sanity_check_32 (xs : t) : unit =
  let dbg = Dumper.dump xs in
  let rec aux bit xs =
    match xs with
    | [] -> ()
    | (x,bit2)::xs ->
        let size = bit - bit2 in
        (match () with
        | _ when bit2 >= bit -> failwith ("composed word not sorted: " ^ dbg)
        | _ when x < 0       -> failwith (spf "negative value %d: %s" x dbg)
        | _ when size <= 0   -> failwith (spf "no space for value %d: %s" x dbg)
        (* if size = 2 then maxval = 3 so x >= 2^2 (= 1 lsl 2) then error *)
        | _ when x >= 1 lsl size ->
            failwith (spf "value %d overflow outside its space (%d - %d): %s "
                       x bit bit2 dbg)
        | _ -> ()
        );
        aux bit2 xs
  in
  aux 32 xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let int_of_bits32 (xs : int32) : int =
  sanity_check_32 xs;
  xs |> List.fold_left (fun acc (v, i) ->
    (v lsl i) lor acc
  ) 0
