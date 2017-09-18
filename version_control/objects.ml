(*s: version_control/objects.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type Objects.t *)
type t = 
  | Blob   of Blob.t
  | Commit of Commit.t
  | Tree   of Tree.t
  (*s: [[Objects.t]] cases *)
  (*  | Tag of Tag.t *)
  (*e: [[Objects.t]] cases *)
(*e: type Objects.t *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(*s: function Objects.read *)
let read ch =
  let str = IO_.read_string_and_stop_char ch ' ' in
  let n = IO_.read_int_and_nullbyte ch in
  let raw = IO.really_nread ch n in
  (* less: assert finished ch? use IO.pos_in? *)
  let ch2 = IO.input_bytes raw in
  (* less: just reuse ch so avoid use of intermediate strings? *)
  match str with
  (*s: [[Objects.read()]] match str cases *)
  | "blob"   -> Blob   (Blob.read ch2)
  (*x: [[Objects.read()]] match str cases *)
  | "tree"   -> Tree   (Tree.read ch2)
  (*x: [[Objects.read()]] match str cases *)
  | "commit" -> Commit (Commit.read ch2)
  (*x: [[Objects.read()]] match str cases *)
  (* "tag" -> Tag (Tag.read raw) *)
  (*e: [[Objects.read()]] match str cases *)
  (* less: assert finished ch2? *)
  | str -> failwith (spf "Objects.read: invalid header: %s" str)
(*e: function Objects.read *)

(*s: function Objects.write *)
let write obj ch =
  let body = 
    IO.output_bytes () |> IO_.with_close_out (fun ch ->
      match obj with
      | Blob x   -> Blob.write x ch
      | Commit x -> Commit.write x ch
      | Tree x   -> Tree.write x ch
      (*s: [[Objects.write()]] match obj cases *)
      (*e: [[Objects.write()]] match obj cases *)
    )
  in
  let header = 
    spf "%s %d\000"
      (match obj with
      | Blob _   -> "blob"
      | Commit _ -> "commit"
      | Tree  _  ->  "tree"
      (*s: [[Objects.write()]] return header, match obj cases *)
      (*e: [[Objects.write()]] return header, match obj cases *)
      ) 
      (Bytes.length body)
  in
  IO.nwrite ch header;
  IO.nwrite ch body
(*e: function Objects.write *)
(*e: version_control/objects.ml *)
