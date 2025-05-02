(*s: version_control/compression.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Stdcompat (* for bytes *)

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
 *  - ocamlgz from ocamlplot, but just an OCaml binding to C lib
 * 
 * Currently used solutions are marked with a '*' above.
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*s: function [[Compression.decompress]] *)
let decompress ch = 
  Unzip.inflate ch
(*e: function [[Compression.decompress]] *)

(*s: function [[Compression.compress]] *)
let compress ic oc =
  Zlib.compress 
    (fun buf -> 
      try IO.input ic buf 0 (Bytes.length buf)
      with IO.No_more_input -> 0
    )
    (fun buf len -> 
      IO.output oc buf 0 len |> ignore)
(*e: function [[Compression.compress]] *)
(*e: version_control/compression.ml *)
