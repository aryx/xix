
type content = bytes

type entry = {
  path: Common.filename;
  mode: Index.mode;
  content: content Lazy.t;
}

type t = 
  | Add of entry
  | Del of entry
  | Modify of entry * entry
