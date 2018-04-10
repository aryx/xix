(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(* for error reporting *)
type loc = {
  file: Common.filename; (* an mkfile *)
  line: int;
}

(* The elements below are not separated by any separator; they are direct
 * concatenations of elements (hence the need for ${name} below).
 * The list must contain at least one element.
 * ex: '%.c' -> W [Percent; String ".c"]
 *)
type word = W of word_element list

  and word_element =
    | String of string (* except the empty string *)
    | Percent
  
    (* evaluated in eval.ml just after parsing *)
    | Var of var
     (* stricter: backquotes are allowed only in word context, not at toplevel
      * so no `echo '<foo.c'` *)
     (* `...` or `{...} (the string does not include the backquote or braces) *)
    | Backquoted of string

     and var = 
      (* $name or ${name} (the string does not contain the $ or {}) *)
      | SimpleVar of string
      (* ${name:a%b=c%d} *)
      | SubstVar of (string * word * word list)

(* Words are separated by spaces. See also Env.values *)
type words = word list


(* (the strings do not contain the leading space nor trailing newline) *)
type recipe = R of string list

(* See also Rules.rule_exec *)
type rule = {
  targets: words;
  prereqs: words;
  attrs: rule_attribute list;
  recipe: recipe option;
}
  and rule_attribute = 
    | Virtual
    | Quiet
    | Delete
    | Interactive (* pad: I added this one *)
    | NotHandled of char


type instr = {
  instr: instr_kind;
  loc: loc;
}

  and instr_kind =
    (* should resolve to a single filename
     * less: could enforce of word? *)
    | Include of words
    (* the words can contain variables, ex: <|rc ../foo.rc $CONF 
     * less: we could also do PipeInclude of recipe I think *)
    | PipeInclude of words

    | Rule of rule

    (* stricter: no dynamic def like X=AVAR  $X=42 ... $AVAR, 
     * so 'string' below, not 'word' *)
    | Definition of string * words
