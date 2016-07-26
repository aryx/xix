exception Parse_error


type stateid = S of int
type nonterm = NT of string
type rule_action = RA of string
type action = 
  | Shift of stateid
  | Reduce of int (* size of rhs of the rule *) * nonterm * rule_action
  | Accept

type 'tok lr_tables = {
  action: stateid * 'tok -> action;
  goto: stateid * nonterm -> stateid;
}

let yyparse_simple lrtables lexfun lexbuf =
  
  let stack = Stack.create () in
  stack |> Stack.push (S 0);
  let a = ref (lexfun lexbuf) in

  let finished = ref false in
  while not !finished do

    let s = Stack.top stack in
    match lrtables.action (s, !a) with
    | Shift t ->
        stack |> Stack.push t;
        a := lexfun lexbuf;
    | Reduce (n, nt, ra) ->
        for i = 1 to n do
          Stack.pop stack |> ignore
        done;
        let s = Stack.top stack in
        stack |> Stack.push (lrtables.goto (s, nt));
        let (NT ntstr) = nt in
        let (RA rastr) = ra in
        print_string (Printf.sprintf "REDUCE %s, ra = %s\n" ntstr rastr);
    | Accept ->
        print_string "DONE\n";
        finished := true
  done  
