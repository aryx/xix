open Common

(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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
(* Some of the code below derives from ocaml-lz77 at
 * https://github.com/samoht/ocaml-lz77
 * I mainly replaced the use of the Cstruct module by the simpler Bytes. 
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type key = (char * char * char) option

(* TODO: int list? char list? *)
type table = (key, int list) Hashtbl.t

type elt =
  | Buffer of bytes
  | Insert of int (* past *) * int (* len *)

type elts = elt list

let window_size = 32 * 1024 (* 1 lsl 15 *)
let buffer_size = 1 lsl 16

type window = {
  mutable wbuffer : bytes;
  mutable wpos : int;
  (* wcrc : adler32; *)
}


let window_create () = {
    wbuffer = Bytes.create buffer_size;
    wpos = 0;
    (*wcrc = adler32_create(); *)
  }

type t = {
  (** input *)
  zinput: IO.input;

  (** output *)
  mutable zoutput   : bytes;
  mutable zoutpos   : int;
  mutable zneeded: int;

  (* todo: zhuffman and zhuffman_dist *)
  (* buffered input *)
  zwindow  : window;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let key buf i =
  if i < Bytes.length buf - 3 then
    let x = Bytes.get buf i in
    let y = Bytes.get buf (i+1) in
    let z = Bytes.get buf (i+2) in
    Some (x, y, z)
  else
    None

let find tbl x =
  try Hashtbl.find tbl x
  with Not_found -> []

let add tbl x off =
  let l = find tbl x in
  Hashtbl.replace tbl x (off :: l)

(* less: handle the optimization described in deflate article
 * where can have len bigger than past string because fill as you go.
 * See tests/blah.txt that should lead to 'Bhah b[D=5;L=18]!'
 *)

let longuest_substring buf i j =
  let rec aux acc len =
    if i + len < j  (* FIXME valid but not allowed by the original algorithm *)
    && j + len < Bytes.length buf
    && Bytes.get buf (i+len) = Bytes.get buf (j+len)
    then aux (Some (len + 1)) (len+1)
    else acc
  in
  aux None 0

let max_insert a b =
  match a, b with
  | Some (_, x), Some (_, y) -> if x >= y then a else b
  | Some _     , None        -> a
  | None       , Some _      -> b
  | None       , None        -> None

let compress_offset tbl buf off =
  let key = key buf off in
  let candidates = find tbl key in
  let rec aux acc = function
    | []   -> acc
    | i::t ->
      if i >= off || off - i > window_size 
      then acc
      else 
        (match longuest_substring buf i off with
        | None     -> aux acc t
        | Some len -> aux (max_insert acc (Some (i, len))) t
        )
  in
  match aux None candidates with
  | None          -> None
  | Some (i, len) -> Some (off - i, len)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let compress buf =
  let res = ref [] in
  let off = ref 0 in
  let len = Bytes.length buf in
  let tbl = Hashtbl.create 1024 in
  let last = ref 0 in
  let flush_last () =
    if !last <> 0 then (
      let s = Bytes.sub buf (!off - !last) !last in
      last := 0;
      res := Buffer s :: !res;
    )
  in
  while !off < len do
    match compress_offset tbl buf !off with
    | None ->
      add tbl (key buf !off) !off;
      incr last;
      incr off
    | Some (start, len) ->
      for i = !off to !off + len - 1 do add tbl (key buf i) i  done;
      flush_last ();
      res := Insert (start, len) :: !res;
      off := !off + len
  done;
  flush_last ();
  List.rev !res

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let deflate_init ch = 
  { 
    zinput = ch;
    
    zoutput = Bytes.empty;
    zoutpos = 0;
    zneeded = 0;
    
    zwindow = window_create ();
  }

let deflate_data z s pos len =
  raise Todo

let deflate ch = 
  let _z = deflate_init ch in

  let buf = IO.read_all ch in
  let xs = compress buf in
  let str = 
    xs |> List.map (function
      | Buffer s -> s
      | Insert(i1, i2) -> spf "[%d;%d]" i1 i2
    ) |> String.concat ""
  in
  IO.input_string str
