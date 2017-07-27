
type content = string

type entry = {
  path: string;
  mode: Index.mode;
  content: content;
}

type t = 
  | Add of entry
  | Del of entry
  | Modify of entry * entry
