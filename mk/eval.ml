open Common

module A = Ast
module R = Rules
module E = Env

let error loc s =
  failwith (spf "%s:%d: Semantic error, %s" loc.A.file loc.A.line s)

let warning loc s =
  pr2 (spf "warning: %s (at %s:%d)" s loc.A.file loc.A.line)


let rec eval_partial_word loc env word =
  word |> List.map (fun word_elem ->
    match word_elem with
    | A.String _ | A.Percent -> [word_elem]
    | A.Var ((A.SimpleVar v | A.SubstVar (v, _, _)) as x)  ->
         (* stricter: mk does not complain *)
         if not (Hashtbl.mem env.E.vars v)
         then error loc (spf "variable not found '%s'" v);

         let xs = Hashtbl.find env.E.vars v in
         let xs =
           match x with
           | A.SimpleVar _ -> xs
           | A.SubstVar (_, pattern, subst) -> 
               let pattern = eval_partial_word loc env pattern in
               let subst = eval_partial_word loc env subst in
               xs |> List.map (fun s -> 
                 Percent.match_and_subst pattern subst s
               )
         in
         xs |> List.map (fun s -> A.String s)
            
    | A.Backquoted s -> error loc "TODO Backquoted not supported yet in eval"

  ) |> List.flatten


(* opti? could use a Buffer *)
let rec eval_word loc env word =
  let word = eval_partial_word loc env word in

  let rec aux acc elems =
    match elems with
    | [] -> [acc]
    | x::xs ->
      (match x with
      | A.String s -> aux (acc ^ s) xs
      (* less: could print a warning? user should escape this char *)
      | A.Percent -> aux (acc ^ "%") xs

      | A.Var _ | A.Backquoted _ -> 
        error loc "Impossible, eval_partial_word should fix Var and Backquoted"
      )
  in
  aux "" word

let rec eval_words loc env words =
  words |> List.map (eval_word loc env) |> List.flatten




let eval env targets xs =

  let simples = Hashtbl.create 101 in
  let metas = ref [] in

  let rec instrs xs = 
    xs |> List.iter (fun instr ->
      let loc = instr.A.loc in
      match instr.A.instr with
      | A.Include ws ->
          let xs = eval_words loc env ws in
          (match xs with
          | [file] -> 
              if not (Sys.file_exists file)
              then warning loc (spf "skipping missing include file: %s" file);
            
              let xs = Parse.parse file in
              (* recurse *)
              instrs xs

          (* new? what does mk does? *)
          | [] -> error loc "missing include file"
          | _ -> error loc "too many files to include"
          )
      | A.Definition (s, ws) ->
          (* todo: handle override variables *)

          (* stricter? forbid redefinitions *)
          if Hashtbl.mem env.E.vars s
          then error loc (spf "redefinition of %s" s);

          let xs = eval_words loc env ws in
          Hashtbl.replace env.E.vars s xs

      | A.Rule r -> error loc "TODO Rule"

    )
  in
  instrs xs;

  { R.
    simples = simples;
    metas = !metas
  }, env

    


