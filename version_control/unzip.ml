(*s: version_control/unzip.ml *)
(*s: copyright ocaml-unzip *)
(*
 * Unzip - inflate format decompression algorithm
 * Copyright (C) 2004 Nicolas Cannasse
 * Compliant with RFC 1950 and 1951
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
(*e: copyright ocaml-unzip *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let debug = ref false

(*s: type [[Unzip.huffman]] *)
type huffman =
  (** Leaf *)
  | Found of int (** 0..288 *)
  (** Node *)
  | NeedBit of huffman * huffman
  (*s: [[Unzip.huffman]] cases *)
  (** Opti *)
  | NeedBits of int * huffman array
  (*e: [[Unzip.huffman]] cases *)
(*e: type [[Unzip.huffman]] *)


(*s: type [[Unzip.adler32]] *)
type adler32 = {
  mutable a1 : int;
  mutable a2 : int;
}
(*e: type [[Unzip.adler32]] *)

(*s: type [[Unzip.window]] *)
type window = {
  mutable wbuffer : bytes;
  mutable wpos : int;
  (*s: [[Unzip.window]] other fields *)
  wcrc : adler32;
  (*e: [[Unzip.window]] other fields *)
}
(*e: type [[Unzip.window]] *)

(*s: type [[Unzip.state]] *)
type state =
  | Head

  | Block
  | CData

  | Done
  (*s: [[Unzip.state]] other cases *)
  | Dist
  (*x: [[Unzip.state]] other cases *)
  | Crc
  (*x: [[Unzip.state]] other cases *)
  | Flat
  (*x: [[Unzip.state]] other cases *)
  | DistOne
  (*e: [[Unzip.state]] other cases *)
(*e: type [[Unzip.state]] *)

(*s: type [[Unzip.t]] *)
type t = {
  mutable zstate    : state;

  mutable zhuffman  : huffman;
  zwindow  : window;

  (** input *)
  zinput   : IO.input;

  (** output *)
  mutable zoutput   : bytes;
  mutable zoutpos   : int;
  mutable zneeded   : int;

  (*s: [[Unzip.t]] other fields *)
  mutable zfinal    : bool;
  (*x: [[Unzip.t]] other fields *)
  (** usually a byte, but can contained more *)
  mutable zbits     : int;
  (** unread (not consumed) bits in zbits (usually 0..8, but can be more) *)
  mutable znbits    : int;
  (*x: [[Unzip.t]] other fields *)
  mutable zlen      : int;
  (*x: [[Unzip.t]] other fields *)
  mutable zdist     : int;
  (*x: [[Unzip.t]] other fields *)
  zlengths : int array;
  (*x: [[Unzip.t]] other fields *)
  mutable zhuffdist : huffman option;
  (*e: [[Unzip.t]] other fields *)
}
(*e: type [[Unzip.t]] *)

(*s: type [[Unzip.error_msg]] *)
type error_msg =
  | Invalid_huffman
  | Invalid_data
  | Invalid_crc
  | Truncated_data
  | Unsupported_dictionary
(*e: type [[Unzip.error_msg]] *)

(*s: exception [[Unzip.Error]] *)
exception Error of error_msg
(*e: exception [[Unzip.Error]] *)

(*s: function [[Unzip.error]] *)
let error msg = raise (Error msg)
(*e: function [[Unzip.error]] *)

(* ************************************************************************ *)
(* HUFFMAN TREES *)

(*s: function [[Unzip.tree_depth]] *)
let rec tree_depth = function
  | Found _ -> 0
  | NeedBit (a,b) ->
    1 + min (tree_depth a) (tree_depth b)
  | NeedBits _ -> assert false
(*e: function [[Unzip.tree_depth]] *)

(*s: function [[Unzip.tree_compress]] *)
let rec tree_compress t =
  match tree_depth t with
  | 0 -> t
  | 1 ->
    (match t with
    | NeedBit (a,b) -> NeedBit (tree_compress a,tree_compress b)
    | _ -> assert false)
  | d ->
    let size = 1 lsl d in
    let tbl = Array.make size (Found (-1)) in
    tree_walk tbl 0 0 d t;
    NeedBits (d,tbl)

and tree_walk tbl p cd d = function
  | NeedBit (a,b) when d > 0 ->
    tree_walk tbl p (cd + 1) (d-1) a;
    tree_walk tbl (p lor (1 lsl cd)) (cd + 1) (d-1) b;
  | t ->
    Array.set tbl p (tree_compress t)
(*e: function [[Unzip.tree_compress]] *)

(*s: function [[Unzip.make_huffman]] *)
let make_huffman lengths pos nlengths maxbits =

  let counts = Array.make maxbits 0 in
  for i = 0 to nlengths - 1 do
    let p = Array.unsafe_get lengths (i + pos) in
    (*s: [[Unzip.make_huffman()]] sanity check codelength [[p]] *)
    if p >= maxbits then error Invalid_huffman;
    (*e: [[Unzip.make_huffman()]] sanity check codelength [[p]] *)
    Array.unsafe_set counts p (Array.unsafe_get counts p + 1);
  done;

  let code = ref 0 in
  let tmp = Array.make maxbits 0 in
  for i = 1 to maxbits - 2 do
    code := (!code + Array.unsafe_get counts i) lsl 1;
    Array.unsafe_set tmp i !code;
  done;

  let bits = Hashtbl.create 0 in
  for i = 0 to nlengths - 1 do
    let l = Array.unsafe_get lengths (i + pos) in
    if l <> 0 then begin
      let n = Array.unsafe_get tmp (l - 1) in
      Array.unsafe_set tmp (l - 1) (n + 1);
      Hashtbl.add bits (n,l) i;
    end;
  done;

  (*s: function [[Unzip.tree_make]] *)
  let rec tree_make v l =
    (*s: [[Unzip.tree_make()]] sanity check [[l]] *)
    if l > maxbits then error Invalid_huffman;
    (*e: [[Unzip.tree_make()]] sanity check [[l]] *)
    try
      Found (Hashtbl.find bits (v,l))
    with
      Not_found ->
        NeedBit (tree_make (v lsl 1) (l + 1) , tree_make (v lsl 1 lor 1) (l + 1))
  in
  (*e: function [[Unzip.tree_make]] *)
    (NeedBit (tree_make 0 1, tree_make 1 1))
(*e: function [[Unzip.make_huffman]] *)

(* ************************************************************************ *)
(* ADLER32 (CRC) *)

(*s: function [[Unzip.adler32_create]] *)
let adler32_create() = {
  a1 = 1;
  a2 = 0;
}
(*e: function [[Unzip.adler32_create]] *)

(*s: function [[Unzip.adler32_update]] *)
let adler32_update a s p l =
  let p = ref p in
  for i = 0 to l - 1 do
    let c = int_of_char (Bytes.unsafe_get s !p) in
    a.a1 <- (a.a1 + c) mod 65521;
    a.a2 <- (a.a2 + a.a1) mod 65521;
    incr p;
  done
(*e: function [[Unzip.adler32_update]] *)

(*s: function [[Unzip.adler32_read]] *)
let adler32_read ch =
  let a2a = IO.read_byte ch in
  let a2b = IO.read_byte ch in
  let a1a = IO.read_byte ch in
  let a1b = IO.read_byte ch in
  {
    a1 = (a1a lsl 8) lor a1b;
    a2 = (a2a lsl 8) lor a2b;
  }
(*e: function [[Unzip.adler32_read]] *)

(* ************************************************************************ *)
(* WINDOW *)

(*s: constant [[Unzip.window_size]] *)
let window_size = 1 lsl 15
(*e: constant [[Unzip.window_size]] *)
(*s: constant [[Unzip.buffer_size]] *)
let buffer_size = 1 lsl 16
(*e: constant [[Unzip.buffer_size]] *)

(*s: function [[Unzip.window_create]] *)
let window_create () = {
    wbuffer = Bytes.create buffer_size;
    wpos = 0;
    (*s: [[Unzip.window_create()]] set other fields *)
    wcrc = adler32_create();
    (*e: [[Unzip.window_create()]] set other fields *)
  }
(*e: function [[Unzip.window_create]] *)

(*s: function [[Unzip.window_slide]] *)
let window_slide w = 
  (*s: [[Unzip.window_slide()]] update crc before blit *)
  adler32_update w.wcrc w.wbuffer 0 window_size;
  (*e: [[Unzip.window_slide()]] update crc before blit *)
  let b = Bytes.create buffer_size in
  w.wpos <- w.wpos - window_size;
  Bytes.unsafe_blit w.wbuffer window_size b 0 w.wpos;
  w.wbuffer <- b
(*e: function [[Unzip.window_slide]] *)

(*s: function [[Unzip.window_add_bytes]] *)
let window_add_bytes w s p len =
  (*s: [[Unzip.window_add_bytes()]] slide window if reached end of buffer *)
  if w.wpos + len > buffer_size 
  then window_slide w;
  (*e: [[Unzip.window_add_bytes()]] slide window if reached end of buffer *)
  Bytes.unsafe_blit s p w.wbuffer w.wpos len;
  w.wpos <- w.wpos + len
(*e: function [[Unzip.window_add_bytes]] *)

(*s: function [[Unzip.window_add_char]] *)
let window_add_char w c =
  (*s: [[Unzip.window_add_char()]] slide window if reached end of buffer *)
  if w.wpos = buffer_size 
  then window_slide w;
  (*e: [[Unzip.window_add_char()]] slide window if reached end of buffer *)
  Bytes.unsafe_set w.wbuffer w.wpos c;
  w.wpos <- w.wpos + 1
(*e: function [[Unzip.window_add_char]] *)

(*s: function [[Unzip.window_get_last_char]] *)
let window_get_last_char w =
  Bytes.unsafe_get w.wbuffer (w.wpos - 1)
(*e: function [[Unzip.window_get_last_char]] *)

(*s: function [[Unzip.window_available]] *)
let window_available w =
  w.wpos
(*e: function [[Unzip.window_available]] *)

(*s: function [[Unzip.window_checksum]] *)
let window_checksum w =
  adler32_update w.wcrc w.wbuffer 0 w.wpos;
  w.wcrc
(*e: function [[Unzip.window_checksum]] *)

(* ************************************************************************ *)

(*s: constant [[Unzip.len_extra_bits_tbl]] *)
let len_extra_bits_tbl = [|0;0;0;0;0;0;0;0;1;1;1;1;2;2;2;2;3;3;3;3;4;4;4;4;5;5;5;5;0;-1;-1|]
(*e: constant [[Unzip.len_extra_bits_tbl]] *)
(*s: constant [[Unzip.len_base_val_tbl]] *)
let len_base_val_tbl = [|3;4;5;6;7;8;9;10;11;13;15;17;19;23;27;31;35;43;51;59;67;83;99;115;131;163;195;227;258|]
(*e: constant [[Unzip.len_base_val_tbl]] *)
(*s: constant [[Unzip.dist_extra_bits_tbl]] *)
let dist_extra_bits_tbl = [|0;0;0;0;1;1;2;2;3;3;4;4;5;5;6;6;7;7;8;8;9;9;10;10;11;11;12;12;13;13;-1;-1|]
(*e: constant [[Unzip.dist_extra_bits_tbl]] *)
(*s: constant [[Unzip.dist_base_val_tbl]] *)
let dist_base_val_tbl = [|1;2;3;4;5;7;9;13;17;25;33;49;65;97;129;193;257;385;513;769;1025;1537;2049;3073;4097;6145;8193;12289;16385;24577|]
(*e: constant [[Unzip.dist_base_val_tbl]] *)
(*s: constant [[Unzip.code_lengths_pos]] *)
let code_lengths_pos = [|16;17;18;0;8;7;9;6;10;5;11;4;12;3;13;2;14;1;15|]
(*e: constant [[Unzip.code_lengths_pos]] *)

(*s: constant [[Unzip.fixed_huffman]] *)
let fixed_huffman = 
  make_huffman (Array.init 288 (fun n ->
    if n <= 143 then 8
    else if n <= 255 then 9
    else if n <= 279 then 7
    else 8
  )) 0 288 10
(*e: constant [[Unzip.fixed_huffman]] *)

(*s: function [[Unzip.get_bits]] *)
let get_bits z n =
  while z.znbits < n do
    z.zbits <- z.zbits lor ((IO.read_byte z.zinput) lsl z.znbits);
    z.znbits <- z.znbits + 8;
  done;
  let b = z.zbits land (1 lsl n - 1) in
  z.znbits <- z.znbits - n;
  z.zbits <- z.zbits lsr n;
  b
(*e: function [[Unzip.get_bits]] *)

(*s: function [[Unzip.get_bit]] *)
let get_bit z =
  if z.znbits = 0 then begin
    z.znbits <- 8;
    z.zbits <- IO.read_byte z.zinput;
  end;
  let b = z.zbits land 1 = 1 in
  z.znbits <- z.znbits - 1;
  z.zbits <- z.zbits lsr 1;
  b
(*e: function [[Unzip.get_bit]] *)

(*s: function [[Unzip.get_rev_bits]] *)
let rec get_rev_bits z n =
  if n = 0 then
    0
  else if get_bit z then
    (1 lsl (n - 1)) lor (get_rev_bits z (n-1))
  else
    get_rev_bits z (n-1)
(*e: function [[Unzip.get_rev_bits]] *)

(*s: function [[Unzip.reset_bits]] *)
let reset_bits z =
  z.zbits <- 0;
  z.znbits <- 0
(*e: function [[Unzip.reset_bits]] *)

(*s: function [[Unzip.add_bytes]] *)
let add_bytes z s p l =
  (*s: [[Unzip.add_bytes()]] add bytes to window *)
  window_add_bytes z.zwindow s p l;
  (*e: [[Unzip.add_bytes()]] add bytes to window *)
  Bytes.unsafe_blit s p z.zoutput z.zoutpos l;
  z.zneeded <- z.zneeded - l;
  z.zoutpos <- z.zoutpos + l
(*e: function [[Unzip.add_bytes]] *)

(*s: function [[Unzip.add_char]] *)
let add_char z c =
  (*s: [[Unzip.add_char()]] add character to window *)
  window_add_char z.zwindow c;
  (*e: [[Unzip.add_char()]] add character to window *)
  Bytes.unsafe_set z.zoutput z.zoutpos c;
  z.zneeded <- z.zneeded - 1;
  z.zoutpos <- z.zoutpos + 1
(*e: function [[Unzip.add_char]] *)

(*s: function [[Unzip.add_dist_one]] *)
let add_dist_one z n =
  let c = window_get_last_char z.zwindow in
  let s = Bytes.make n c in
  add_bytes z s 0 n
(*e: function [[Unzip.add_dist_one]] *)

(*s: function [[Unzip.add_dist]] *)
let add_dist z d l =
  add_bytes z z.zwindow.wbuffer (z.zwindow.wpos - d) l
(*e: function [[Unzip.add_dist]] *)

(*s: function [[Unzip.apply_huffman]] *)
let rec apply_huffman z = function
  | Found n -> n
  | NeedBit (a,b) -> apply_huffman z (if get_bit z then b else a)
 (*s: [[Unzip.apply_huffman()]] other cases *)
 | NeedBits (n,t) -> apply_huffman z (Array.unsafe_get t (get_bits z n))
 (*e: [[Unzip.apply_huffman()]] other cases *)
(*e: function [[Unzip.apply_huffman]] *)

(*s: function [[Unzip.inflate_lengths]] *)
let inflate_lengths z a max =
  let i = ref 0 in
  let prev = ref 0 in
  while !i < max do
    match apply_huffman z z.zhuffman with
    | n when n <= 15 ->
      prev := n;
      Array.unsafe_set a !i n;
      incr i
    | 16 ->
      let n = 3 + get_bits z 2 in
      if !i + n > max then error Invalid_data;
      for k = 0 to n - 1 do
        Array.unsafe_set a !i !prev;
        incr i;
      done;
    | 17 ->
      let n = 3 + get_bits z 3 in
      i := !i + n;
      if !i > max then error Invalid_data;
    | 18 ->
      let n = 11 + get_bits z 7 in
      i := !i + n;
      if !i > max then error Invalid_data;
    | _ ->
      error Invalid_data
  done
(*e: function [[Unzip.inflate_lengths]] *)

(*s: function [[Unzip.inflate_loop]] *)
let rec inflate_loop z =
  match z.zstate with
  (*s: [[Unzip.inflate_loop()]] match state cases *)
  | Head ->
    let cmf = IO.read_byte z.zinput in
    (*s: [[Unzip.inflate_loop()]] when in Head state, sanity check [[cmf]] *)
    let cm     = cmf land 15 in
    let cinfo  = cmf lsr 4 in
    if cm <> 8 || cinfo <> 7 
    then error Invalid_data;
    (*e: [[Unzip.inflate_loop()]] when in Head state, sanity check [[cmf]] *)
    let flg = IO.read_byte z.zinput in
    (*s: [[Unzip.inflate_loop()]] when in Head state, sanity check [[flg]] *)
    let fdict = flg land 32 <> 0 in
    if (cmf lsl 8 + flg) mod 31 <> 0 
    then error Invalid_data;
    if fdict 
    then error Unsupported_dictionary;
    (*e: [[Unzip.inflate_loop()]] when in Head state, sanity check [[flg]] *)
    z.zstate <- Block;
    inflate_loop z
  (*x: [[Unzip.inflate_loop()]] match state cases *)
  | Block ->
    z.zfinal <- get_bit z;
    let btype = get_bits z 2 in
    (match btype with
    (*s: [[Unzip.inflate_loop()]] when in Block state, match block type cases *)
    | 1 -> (* fixed Huffman *)
      if !debug then print_string "Unzip: Fixed Huffman\n";
      z.zhuffman <- fixed_huffman;
      z.zhuffdist <- None;
      z.zstate <- CData;
      inflate_loop z
    (*x: [[Unzip.inflate_loop()]] when in Block state, match block type cases *)
    | 0 -> (* no compression *)
      if !debug then print_string "Unzip: no compression\n";
      z.zlen <- IO.LittleEndian.read_ui16 z.zinput;
      let nlen = IO.LittleEndian.read_ui16 z.zinput in
      if nlen <> 0xffff - z.zlen 
      then error Invalid_data;
      z.zstate <- Flat;
      inflate_loop z;
      reset_bits z
    (*x: [[Unzip.inflate_loop()]] when in Block state, match block type cases *)
    | 2 -> (* dynamic Huffman *)
      if !debug then print_string "Unzip: Dynamic Huffman\n";
      let hlit = get_bits z 5 + 257 in
      let hdist = get_bits z 5 + 1 in
      let hclen = get_bits z 4 + 4 in
      for i = 0 to hclen - 1 do
        Array.unsafe_set z.zlengths (Array.unsafe_get code_lengths_pos i) (get_bits z 3);
      done;
      for i = hclen to 18 do
        Array.unsafe_set z.zlengths (Array.unsafe_get code_lengths_pos i) 0;
      done;
      z.zhuffman <- make_huffman z.zlengths 0 19 8;
      let lengths = Array.make (hlit + hdist) 0 in
      inflate_lengths z lengths (hlit + hdist);
      z.zhuffdist <- Some (make_huffman lengths hlit hdist 16);
      z.zhuffman <- make_huffman lengths 0 hlit 16;      
      z.zstate <- CData;
      inflate_loop z
    (*e: [[Unzip.inflate_loop()]] when in Block state, match block type cases *)
    | _ ->
      error Invalid_data
    )
  (*x: [[Unzip.inflate_loop()]] match state cases *)
  | CData ->
    (match apply_huffman z z.zhuffman with
    (*s: [[Unzip.inflate_loop()]] when in CData state, match apply huffman cases *)
    | n when n < 256 ->
      add_char z (Char.unsafe_chr n);
      if z.zneeded > 0  
      then inflate_loop z
    (*x: [[Unzip.inflate_loop()]] when in CData state, match apply huffman cases *)
    | 256 ->
      z.zstate <- if z.zfinal then Crc else Block;
      inflate_loop z
    (*x: [[Unzip.inflate_loop()]] when in CData state, match apply huffman cases *)
    | n ->
      let n = n - 257 in
      (*s: [[Unzip.inflate_loop()]] when in CData state, when backref, compute zlen *)
      let extra_bits = Array.unsafe_get len_extra_bits_tbl n in
      if extra_bits = -1 
      then error Invalid_data;
      z.zlen <- (Array.unsafe_get len_base_val_tbl n) + (get_bits z extra_bits);
      (*e: [[Unzip.inflate_loop()]] when in CData state, when backref, compute zlen *)
      (*s: [[Unzip.inflate_loop()]] when in CData state, when backref, compute zdist *)
      let dist_code = 
        match z.zhuffdist with 
        | None -> get_rev_bits z 5 
        (*s: [[Unzip.inflate_loop()]] compute [[dist_code]], match [[zhuffdist]] cases *)
        | Some h -> 
          apply_huffman z h
        (*e: [[Unzip.inflate_loop()]] compute [[dist_code]], match [[zhuffdist]] cases *)
      in
      let extra_bits = Array.unsafe_get dist_extra_bits_tbl dist_code in
      if extra_bits = -1 
      then error Invalid_data;
      z.zdist <- (Array.unsafe_get dist_base_val_tbl dist_code) + (get_bits z extra_bits);
      (*e: [[Unzip.inflate_loop()]] when in CData state, when backref, compute zdist *)

      if z.zdist > window_available z.zwindow 
      then error Invalid_data;
      z.zstate <- Dist;
      inflate_loop z
    (*e: [[Unzip.inflate_loop()]] when in CData state, match apply huffman cases *)
    )
  (*x: [[Unzip.inflate_loop()]] match state cases *)
  | Dist ->
    while z.zlen > 0 && z.zneeded > 0 do
      let len = min z.zneeded (min z.zlen z.zdist) in
      add_dist z z.zdist len;
      z.zlen <- z.zlen - len;
    done;
    if z.zlen = 0 
    then z.zstate <- CData;
    if z.zneeded > 0 
    then inflate_loop z
  (*x: [[Unzip.inflate_loop()]] match state cases *)
  | Crc ->
    (*s: [[Unzip.inflate_loop()]] when in Crc state, check adler32 checksum *)
    let calc = window_checksum z.zwindow in
    let crc = adler32_read z.zinput in
    if calc <> crc 
    then error Invalid_crc;
    (*e: [[Unzip.inflate_loop()]] when in Crc state, check adler32 checksum *)
    z.zstate <- Done;
    inflate_loop z
  (*x: [[Unzip.inflate_loop()]] match state cases *)
  | Done ->
    ()
  (*x: [[Unzip.inflate_loop()]] match state cases *)
  | Flat ->
    let rlen = min z.zlen z.zneeded in
    let str = IO.nread z.zinput rlen in
    let len = Bytes.length str in
    z.zlen <- z.zlen - len;
    add_bytes z str 0 len;
    if z.zlen = 0 
    then z.zstate <- (if z.zfinal then Crc else Block);
    if z.zneeded > 0 
    then inflate_loop z
  (*x: [[Unzip.inflate_loop()]] match state cases *)
  | DistOne ->
    if !debug then print_string "Unzip: DistOne\n";
    let len = min z.zlen z.zneeded in
    add_dist_one z len;
    z.zlen <- z.zlen - len;
    if z.zlen = 0 
    then z.zstate <- CData;
    if z.zneeded > 0 
    then inflate_loop z
  (*e: [[Unzip.inflate_loop()]] match state cases *)
(*e: function [[Unzip.inflate_loop]] *)

(*s: function [[Unzip.inflate_data]] *)
let inflate_data z buf_dst pos_in_buf len =
  (*s: [[Unzip.inflate_data()]] sanity check parameters *)
  if pos_in_buf < 0 || len < 0 || pos_in_buf + len > Bytes.length buf_dst 
  then invalid_arg "inflate_data";
  (*e: [[Unzip.inflate_data()]] sanity check parameters *)
  z.zneeded <- len;
  z.zoutpos <- pos_in_buf;
  z.zoutput <- buf_dst;
  try
    if len > 0 
    then inflate_loop z;
    len - z.zneeded
  with IO.No_more_input -> error Truncated_data
(*e: function [[Unzip.inflate_data]] *)

(*s: function [[Unzip.inflate_init]] *)
let inflate_init ?(header=true) ch = 
  {
    zstate = (if header then Head else Block);
    zinput = ch;
    zfinal = false;

    zoutput = Bytes.empty;
    zoutpos = 0;
    zneeded = 0;

    zhuffman = fixed_huffman;
    zhuffdist = None;

    zwindow = window_create ();

    zbits = 0;
    znbits = 0;

    zlen = 0;
    zdist = 0;
    zlengths = Array.make 19 (-1);
  }
(*e: function [[Unzip.inflate_init]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Unzip.inflate]] *)
let inflate ?(header=true) ch =
  let z = inflate_init ~header ch in
  let tmp = Bytes.create 1 in
  IO.create_in
    ~input:(fun buf_dst pos_in_buf len ->
      let n = inflate_data z buf_dst pos_in_buf len in
      if n = 0 
      then raise IO.No_more_input;
      n
    )
    ~close:(fun () ->
      IO.close_in ch
    )
    (** special case of ~input for 1 byte *)
    ~read:(fun() ->
      let l = inflate_data z tmp 0 1 in
      if l = 1 
      then Bytes.unsafe_get tmp 0 
      else raise IO.No_more_input
    )
(*e: function [[Unzip.inflate]] *)
(*e: version_control/unzip.ml *)
