open Common

module A = Ast
module R = Rules
module E = Env

let error loc s =
  failwith (spf "%s:%d: Semantic error, %s" loc.A.file loc.A.line s)

let warning loc s =
  pr2 (spf "warning: %s (at %s:%d)" s loc.A.file loc.A.line)


(* opti? could use a Buffer *)
let rec eval_word loc env word =
  let rec aux acc elems =
    match elems with
    | [] -> [acc]
    | x::xs ->
      (match x with
      | A.String s -> aux (acc ^ s) xs
      | A.Percent -> aux (acc ^ "%") xs
      | A.Var v ->
        (match v with
        | A.SimpleVar v ->
            (* stricter: mk does not complain *)
            if not (Hashtbl.mem env.E.vars v)
            then error loc (spf "variable not found '%s'" v);

            let ys = Hashtbl.find env.E.vars v in
            (match ys with
            | [] -> aux acc xs
            | [y] -> aux (acc ^ y) xs
            | _ -> error loc "TODO multi strings variable"
            )
        | A.SubstVar _ -> error loc "TODO Substvar not supported yet in eval"
        )
      | A.Backquoted s -> error loc "TODO Backquoted not supported yet in eval"
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

    


