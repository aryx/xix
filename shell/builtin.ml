open Common

module R = Runtime

let is_builtin s =
  List.mem s ["cd"; "exit"]

let dochdir s =
  try 
    Unix.chdir s;
    true
  with Unix.Unix_error _ ->
    false

let dispatch s =
  match s with
  | "cd" -> 
      let t = R.cur () in
      let argv = t.R.argv in
      Status.setstatus "can't cd";
      (* less: cdpath vlook *)
      (match argv with
      | [cd] -> 
          let v = (Var.vlook "home").R.v in
          (match v with
          | Some (dir::_) ->
            if dochdir dir
            then Status.setstatus ""
            (* less: %r *)
            else pr2 (spf "Can't cd %s" dir)
          | _ -> pr2 ("Can't cd -- $home empty")
          )
      | [_cd;dir] ->
          (* less: cdpath iteration *)
          if dochdir dir
          then Status.setstatus ""
          else pr2 (spf "Can't cd %s" dir)
      | _ ->
          pr2 "Usage: cd [directory]";
      );
      R.pop_list()

  | "exit" -> failwith s
  | _ -> failwith (spf "unsupported builtin %s" s)

