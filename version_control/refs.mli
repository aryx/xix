(*s: version_control/refs.mli *)

(*s: type Refs.refname *)
(* should always start with "refs/", see is_refname() later *)
type refname = string (* e.g. "refs/heads/master" *)
(*e: type Refs.refname *)

(*s: type Refs.t *)
type t =
  | Head
  | Ref of refname
(*e: type Refs.t *)

(*s: type Refs.ref_content *)
type ref_content =
  (* the final value when follow all the pointers *)
  | Hash of Commit.hash
  (* pointer (may contain sha1 or another pointer again) *)
  | OtherRef of refname
(*e: type Refs.ref_content *)

(*s: signature Refs.default_head_content *)
val default_head_content: ref_content
(*e: signature Refs.default_head_content *)

(*s: signature Refs.is_valid_refname *)
val is_refname: refname -> bool
(*e: signature Refs.is_valid_refname *)

(*s: signature Refs.read *)
val read: IO.input -> ref_content
(*e: signature Refs.read *)
(*s: signature Refs.write *)
val write: ref_content -> unit IO.output -> unit
(*e: signature Refs.write *)

(*s: signature Refs.string_of_ref *)
val string_of_ref: t -> string
(*e: signature Refs.string_of_ref *)
(*e: version_control/refs.mli *)
