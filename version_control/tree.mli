
type perm = 
  | Normal
  | Exec
  | Link
  | Dir
  | Commit

type entry = {
  perm: perm;
  name: string;
  (* blob or tree *)
  node: Sha1.t;
}

type t = entry list


(* assumes have already read the 'tree <size>\000' header from input *)
val read: IO.input -> t
