(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Compression/decompression wrappers.
 * 
 * alternatives:
 *  * https://github.com/ygrek/ocaml-extlib/src/unzip.ml
 *    just decompression (inflate), pretty small: 450 LOC
 *  - https://github.com/mirage/decompress
 *    compression/decompression, seems complete, but pretty big. 
 *    used by https://github.com/mirage/ocaml-git
 *  * https://github.com/xavierleroy/camlzip
 *    uses C code (some of the code in ocaml-git uses ML code from camlzip)
 *  - https://github.com/madroach/ocaml-zlib
 *    ??
 *  - https://github.com/samoht/ocaml-lz77 
 *    copy-pasted in decompress, but too simple. Does not
 *    support right API where can compress/decompress strings.
 *  - libflate in plan9 :)
 * 
 * Currently used solutions are marked with a '*' above.
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let decompress ch = 
  Unzip.inflate ch

let compress ic oc =
  Zlib.compress 
    (fun buf -> 
      try IO.input ic buf 0 (Bytes.length buf)
      with IO.No_more_input -> 0
    )
    (fun buf len -> 
      IO.output oc buf 0 len |> ignore)
