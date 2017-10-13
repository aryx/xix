(*s: version_control/sha1.ml *)
(*s: copyright uuidm *)
(*
Copyright (c) 2008 Daniel C. Bünzli

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)
(*e: copyright uuidm *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Most of the code below comes from: https://github.com/dbuenzli/uuidm 
 * (this code is also copy-pasted and used in git-mirage)
 * 
 * alternatives:
 *  - https://github.com/vincenthz/ocaml-sha 
 *    implements algorithm in highly optimized C code and then provides
 *    bindings to call this C code
 *  - https://github.com/xavierleroy/cryptokit/
 *    by Xavier Leroy, but also uses C code
 *  - nocrypto
 *    seems like the official crypto lib, but also uses C code
 *  - md5sum, produces 128-bit hash value (sha1 is 20 bytes so 160 bits)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type Sha1.t *)
(* a 20 bytes number (really a string of length 20) *)
type t = bytes
(*e: type Sha1.t *)

(*s: function Sha1.is_sha *)
let is_sha x =
  Bytes.length x = 20
(*e: function Sha1.is_sha *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*s: function Sha1.read *)
let read ch =
  let s = IO.really_nread ch 20 in
  assert (is_sha s);
  s
(*e: function Sha1.read *)

(*s: function Sha1.write *)
let write ch x =
  IO.nwrite ch x
(*e: function Sha1.write *)

(* start of copy-pasted code from uuidm *)

(*s: function Sha1.sha1 *)
(* sha-1 digest. Based on pseudo-code of RFC 3174.
   Slow and ugly but does the job. *)
let sha1 s =
  (*s: function sha_1_pad *)
  let sha_1_pad s =
    let len = String.length s in
    let blen = 8 * len in
    let rem = len mod 64 in
    let mlen = if rem > 55 then len + 128 - rem else len + 64 - rem in
    let m = Bytes.create mlen in
    Bytes.blit_string s 0 m 0 len;
    Bytes.fill m len (mlen - len) '\x00';
    Bytes.set m len '\x80';
    if Sys.word_size > 32 then begin
      Bytes.set m (mlen - 8) (Char.unsafe_chr (blen lsr 56 land 0xFF));
      Bytes.set m (mlen - 7) (Char.unsafe_chr (blen lsr 48 land 0xFF));
      Bytes.set m (mlen - 6) (Char.unsafe_chr (blen lsr 40 land 0xFF));
      Bytes.set m (mlen - 5) (Char.unsafe_chr (blen lsr 32 land 0xFF));
    end;
    Bytes.set m (mlen - 4) (Char.unsafe_chr (blen lsr 24 land 0xFF));
    Bytes.set m (mlen - 3) (Char.unsafe_chr (blen lsr 16 land 0xFF));
    Bytes.set m (mlen - 2) (Char.unsafe_chr (blen lsr 8 land 0xFF));
    Bytes.set m (mlen - 1) (Char.unsafe_chr (blen land 0xFF));
    m
  in
  (*e: function sha_1_pad *)
  let ( &&& ) = ( land ) in
  (* Operations on int32 *)
  (*s: [[Sha1.sha1()]] operator shortcuts for Int32 *)
  let ( lor )  = Int32.logor in
  let ( lxor ) = Int32.logxor in
  let ( land ) = Int32.logand in
  let ( ++ )   = Int32.add in
  let lnot     = Int32.lognot in
  let sr       = Int32.shift_right in
  let sl       = Int32.shift_left in
  let cls n x = (sl x n) lor (Int32.shift_right_logical x (32 - n)) in
  (*e: [[Sha1.sha1()]] operator shortcuts for Int32 *)
  (* Start *)
  let m = sha_1_pad s in
  let w = Array.make 16 0l in

  (** components of the 20 bytes SHA1 (5 * 4 bytes (Int32 with 'l' suffix)) **)
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in

  let a = ref 0l in
  let b = ref 0l in
  let c = ref 0l in
  let d = ref 0l in
  let e = ref 0l in

  (* For each block *)
  for i = 0 to ((Bytes.length m) / 64) - 1 do
    (* Fill w *)
    (*s: [[Sha1.sha1()]] fill [[w]] for each block [[i]] *)
    let base = i * 64 in
    for j = 0 to 15 do
      let k = base + (j * 4) in
      w.(j) <- sl (Int32.of_int (Char.code @@ Bytes.get m k)) 24 lor
               sl (Int32.of_int (Char.code @@ Bytes.get m (k + 1))) 16 lor
               sl (Int32.of_int (Char.code @@ Bytes.get m (k + 2))) 8 lor
               (Int32.of_int (Char.code @@ Bytes.get m (k + 3)))
    done;
    (*e: [[Sha1.sha1()]] fill [[w]] for each block [[i]] *)
    (* Loop *)
    a := !h0; b := !h1; c := !h2; d := !h3; e := !h4;
    (*s: [[Sha1.sha1()]] loop from 0 to 79 for each block [[i]] *)
    for t = 0 to 79 do
      let f, k =
        if t <= 19 then (!b land !c) lor ((lnot !b) land !d), 0x5A827999l else
        if t <= 39 then !b lxor !c lxor !d, 0x6ED9EBA1l else
        if t <= 59 then
          (!b land !c) lor (!b land !d) lor (!c land !d), 0x8F1BBCDCl
        else
        !b lxor !c lxor !d, 0xCA62C1D6l
      in
      let s = t &&& 0xF in
      if (t >= 16) then begin
        w.(s) <- cls 1 begin
            w.((s + 13) &&& 0xF) lxor
            w.((s + 8) &&& 0xF) lxor
            w.((s + 2) &&& 0xF) lxor
            w.(s)
          end
      end;
      let temp = (cls 5 !a) ++ f ++ !e ++ w.(s) ++ k in
      e := !d;
      d := !c;
      c := cls 30 !b;
      b := !a;
      a := temp;
    done;
    (*e: [[Sha1.sha1()]] loop from 0 to 79 for each block [[i]] *)
    (* Update *)
    h0 := !h0 ++ !a;
    h1 := !h1 ++ !b;
    h2 := !h2 ++ !c;
    h3 := !h3 ++ !d;
    h4 := !h4 ++ !e
  done;

  (** the result hash number of 20 bytes *)
  let h = Bytes.create 20 in
  (*s: function i2s *)
  let i2s h k i =
    Bytes.set h k       (Char.unsafe_chr ((Int32.to_int (sr i 24)) &&& 0xFF));
    Bytes.set h (k + 1) (Char.unsafe_chr ((Int32.to_int (sr i 16)) &&& 0xFF));
    Bytes.set h (k + 2) (Char.unsafe_chr ((Int32.to_int (sr i 8))  &&& 0xFF));
    Bytes.set h (k + 3) (Char.unsafe_chr ((Int32.to_int i)         &&& 0xFF));
  in
  (*e: function i2s *)
  i2s h 0 !h0;
  i2s h 4 !h1;
  i2s h 8 !h2;
  i2s h 12 !h3;
  i2s h 16 !h4;
  Bytes.unsafe_to_string h
(*e: function Sha1.sha1 *)
(*e: version_control/sha1.ml *)
