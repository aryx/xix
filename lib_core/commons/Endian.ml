open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Little vs Big Endian input/output.
 * See https://en.wikipedia.org/wiki/Endianness
 *)

(*****************************************************************************)
(* Big *)
(*****************************************************************************)

type t =
  | Big (* BE *)
  | Little (* LE *)

module Big = struct

(* old: was called lput in A_out.ml *)
(* TODO: use Int32.t *)
let output_32 (chan : out_channel) (word : int) : unit =
  if word < 0 && word > 0xffffffff
  then raise (Impossible (spf "should call lput with a uint32 not %d" word));

  (* could also use land 0xFF ? what about negative numbers? *)
  let x1 = Char.chr (word mod 256) in
  let x2 = Char.chr ((word lsr 8) mod 256) in
  let x3 = Char.chr ((word lsr 16) mod 256) in
  let x4 = Char.chr ((word lsr 24) mod 256) in
  (* big part first; most-significant byte first *)
  output_char chan x4;
  output_char chan x3;
  output_char chan x2;
  output_char chan x1;
  ()

(* old: was called wput in A_out.ml *)
let output_16 (chan : out_channel) (word : int) : unit =
  if word < 0 && word > 0xffff
  then raise (Impossible (spf "should call lput with a uint16 not %d" word));

  (* could also use land 0xFF ? what about negative numbers? *)
  let x1 = Char.chr (word mod 256) in
  let x2 = Char.chr ((word lsr 8) mod 256) in
  (* big part first; most-significant byte first *)
  output_char chan x2;
  output_char chan x1;
  ()

end

(*****************************************************************************)
(* Little *)
(*****************************************************************************)

module Little = struct

(* old: was called lputl for little-endian put long in linker/Executable.ml *)
(* TODO: use Int32.t *)
let output_32 (chan : out_channel) (word : int) : unit =
  if word < 0 && word > 0xffffffff
  then raise (Impossible (spf "should call with a uint32 not %d" word));
  (* TODO? sanity check not > 32 bits uint? *)

  let x1 = Char.chr (word mod 256) in
  let x2 = Char.chr ((word lsr 8) mod 256) in
  let x3 = Char.chr ((word lsr 16) mod 256) in
  let x4 = Char.chr ((word lsr 24) mod 256) in
  (* little part first; least-significant byte first *)
  output_char chan x1;
  output_char chan x2;
  output_char chan x3;
  output_char chan x4;
  ()

let output_16 (chan : out_channel) (word : int) : unit =
  if word < 0 && word > 0xffff
  then raise (Impossible (spf "should call with a uint16 not %d" word));
  (* TODO? sanity check not > 32 bits uint? *)

  let x1 = Char.chr (word mod 256) in
  let x2 = Char.chr ((word lsr 8) mod 256) in
  (* little part first; least-significant byte first *)
  output_char chan x1;
  output_char chan x2;
  ()

end

let output_functions_of_endian (endian : t) =
  match endian with
  | Big -> Big.output_16, Big.output_32
  | Little -> Little.output_16, Little.output_32
