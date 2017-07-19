type t =
  | Head (* "HEAD" *)
  (* should always start with "refs/" *)
  | Ref of Common.filename (* e.g. "refs/heads/master" *)

type ref_content =
  (* the final value when follow all the pointers *)
  | Hash of Commit.hash
  (* pointer (may contain sha1 or another pointer again) *)
  | OtherRef of Common.filename
