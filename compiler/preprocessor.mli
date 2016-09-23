
type cmdline_defs = (string * string) list

type include_paths = Common.filename list

type macro = {
  name: string;
  nbargs: int option;
  body: string;
}

type line_history = HistoryTodo
