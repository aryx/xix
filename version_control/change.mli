type entry = {
  path: string;
  mode: Index.mode;
  content: Sha1.t;
}

type t = 
  | Add of entry
  | Del of entry
  | Modify of entry * entry
