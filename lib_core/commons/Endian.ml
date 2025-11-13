open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Little vs Big Endian input/output.
 * See https://en.wikipedia.org/wiki/Endianness
 *
 * Note that OCaml 4.08 introduced many endian-related functions in buffer.mli,
 * bytes.mli, and string.mli. See also Sys.big_endian bool since 4.00.0
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO: use Int32.t *)
let split_32 (word : int) : byte * byte * byte * byte =
  if word < 0 && word > 0xffffffff
  then raise (Impossible (spf "should call lput with a uint32 not %d" word));

  (* could also use land 0xFF ? *)
  let x1 = Char.chr (word mod 256) in
  let x2 = Char.chr ((word lsr 8) mod 256) in
  let x3 = Char.chr ((word lsr 16) mod 256) in
  let x4 = Char.chr ((word lsr 24) mod 256) in
  x1, x2, x3, x4

let split_16 (word : int) : byte * byte =
  if word < 0 && word > 0xffff
  then raise (Impossible (spf "should call lput with a uint16 not %d" word));

  (* could also use land 0xFF ? *)
  let x1 = Char.chr (word mod 256) in
  let x2 = Char.chr ((word lsr 8) mod 256) in
  x1, x2
  
(*****************************************************************************)
(* Big *)
(*****************************************************************************)

type t =
  (* a.k.a. BE, network byte order *)
  | Big 
  (* a.k.a. LE *)
  | Little


module Big = struct

let array_32 (word : int) : byte array =
  let x1, x2, x3, x4 = split_32 word in
  (* big part first; most-significant byte first *)
  [| x4; x3; x2; x1 |]

let array_16 (word : int) : byte array =
  let x1, x2 = split_16 word in
  (* big part first; most-significant byte first *)
  [| x2; x1 |]
  

(* old: was called lput in A_out.ml *)
let output_32 (chan : out_channel) (word : int) : unit =
  array_32 word |> Array.iter (output_char chan)

(* old: was called wput in A_out.ml *)
let output_16 (chan : out_channel) (word : int) : unit =
  array_16 word |> Array.iter (output_char chan)

end

(*****************************************************************************)
(* Little *)
(*****************************************************************************)

module Little = struct

let array_32 ( word : int) : byte array =
  let x1, x2, x3, x4 = split_32 word in
  (* little part first; least-significant byte first *)
  [| x1; x2; x3; x4 |]

let array_16 ( word : int) : byte array =
  let x1, x2 = split_16 word in
  (* little part first; least-significant byte first *)
  [| x1; x2 |]

(* old: was called lputl for little-endian put long in linker/Executable.ml *)
let output_32 (chan : out_channel) (word : int) : unit =
  array_32 word |> Array.iter (output_char chan)

let output_16 (chan : out_channel) (word : int) : unit =
  array_16 word |> Array.iter (output_char chan)

end

let output_functions_of_endian (endian : t) =
  match endian with
  | Big -> Big.output_16, Big.output_32
  | Little -> Little.output_16, Little.output_32

let array_functions_of_endian (endian : t) =
  match endian with
  | Big -> Big.array_16, Big.array_32
  | Little -> Little.array_16, Little.array_32
