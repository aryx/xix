open Common

module R = Runtime


let op_Simple () =
  let t = R.cur () in
  let argv = t.R.argv in

  (* less: globlist () *)
  (* less: -x *)
  match argv with
  (* How can this happen? Simple has at least a word. 
   * But if you do A=()\n and then $A\n then Simple has a word, but after
   * expansion the list becomes empty.
   * stricter: give extra explanations
   *)
  | [] -> R.error "empty argument list (after variable expansion)" 
  | argv0::args ->
      if Builtin.is_builtin argv0
      then raise Todo
      else begin
        raise Todo
      end


