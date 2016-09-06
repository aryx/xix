%{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * todo:
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let mk_Seq (a, b) =
  match a, b with
  | EmptyCommand, _ -> b
  | _, (LastCmd EmptyCommand) -> LastCmd a
  | _ -> Seq (a,b)
  
%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 Keywords *)*/
/*(*-----------------------------------------*)*/
%token TFor TIn  TIf TNot TWhile  TSwitch  
%token TFn

/*(*-----------------------------------------*)*/
/*(*2 Word *)*/
/*(*-----------------------------------------*)*/
%token<string * bool(* quoted *)> TWord

/*(*-----------------------------------------*)*/
/*(*2 Operators *)*/
/*(*-----------------------------------------*)*/
%token TAndAnd TOrOr TBang
%token TPipe
%token<Ast.redirection_kind> TRedir
%token TAnd TSubshell
%token TTwiddle  

/*(*-----------------------------------------*)*/
/*(*2 Punctuation *)*/
/*(*-----------------------------------------*)*/

%token TOPar TCPar TOBrace TCBrace
%token TSemicolon TEq
%token TDollar TCount TStringify 
%token TSub
%token TCaret
%token TBackquote

/*(*-----------------------------------------*)*/
/*(*2 Misc *)*/
/*(*-----------------------------------------*)*/
%token TNewline

%token EOF

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/

/*(* from low to high *)*/
%left TIf TWhile TFor TSwitch TCPar TNot
%left TAndAnd TOrOr
%left TBang TSubshell
%left TPipe
%left TCaret
%right TDollar TCount TStringify
%left TSub

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/

%type <Ast.line> rc
%start rc

%%

/*(*************************************************************************)*/
/*(*1 line *)*/
/*(*************************************************************************)*/
rc:
  | EOF           { None }
  | line TNewline { Some $1 }

/*(* =~ stmt *)*/
line:
  | cmd { LastCmd $1 }
  | cmdsa line { $1 $2  }

cmdsa:
  | cmd TSemicolon { (fun x -> mk_Seq ($1, x)) }
  | cmd TAnd       { (fun x -> Async ($1, x)) }

/*(*************************************************************************)*/
/*(*1 Command *)*/
/*(*************************************************************************)*/

/*(* =~ expr *)*/
cmd:
  | /*empty*/ { EmptyCommand }
  | simple    { 
      let (cmd, args, redirs) = $1 in
      let args = List.rev args in
      let redirs = List.rev redirs in
      let base = Simple (cmd, args) in
      redirs |> List.fold_left (fun acc e ->
        Redir (acc, e)
      ) base
    }

  | cmd TAndAnd cmd { And ($1, $3) }
  | cmd TOrOr cmd   { Or  ($1, $3) }
  | TBang cmd       { Not $2 }
  | TTwiddle word words { Match ($2, $3) }

  | cmd TPipe cmd  { Pipe ($1, $3) }
  | brace epilog   { 
      $2 |> List.fold_left (fun acc e -> Redir (acc, e)) (Compound $1) 
  }

  | TIf paren_skipnl cmd { If ($2, $3) }
  | TIf tnot_skipnl  cmd { IfNot $3 }

  | TWhile paren_skipnl cmd   { While ($2, $3) }
  | TSwitch word_skipnl brace { Switch ($2, $3) }


  | TFor TOPar word TIn words tcpar_skipnl cmd %prec TFor 
     { ForIn ($3, $5, $7) }
  | TFor TOPar word tcpar_skipnl cmd           %prec TFor 
     { For ($3, $5) }

  /*(* stricter: allow only word, not words *)*/
  | TFn word brace { Fn ($2, $3) }
  | TFn word       { DelFn $2 }

  | assign cmd %prec TBang { $1 $2 }



/*(* =~ primary expr *)*/
simple:
  | first        { $1, [], [] }
  | simple word  { let (a,b,c) = $1 in (a, $2::b, c) }
  | simple redir { let (a,b,c) = $1 in (a, b, $2::c) }


paren: TOPar body TCPar { $2 }

brace: TOBrace body TCBrace { $2 }

body: 
  | cmd         { LastCmd $1 }
  | cmdsan body { $1 $2 }

cmdsan:
  | cmdsa        { $1 }
  | cmd TNewline { (fun x -> mk_Seq ($1, x)) }

assign: first TEq word { (fun x -> Assign ($1, $3, x))  }


/*(*************************************************************************)*/
/*(*1 Word *)*/
/*(*************************************************************************)*/

first:
  | comword           { $1 }
  | first TCaret word { Concat ($1, $3) }


comword:
  | TWord        { Word (fst $1, snd $1) }
  | TDollar word { Dollar $2 }
  | TCount word  { Count $2 }
  | TDollar word TSub words TCPar { Index ($2, $4) }
  | TOPar words TCPar  { List $2 }
  /* less: TStringify
   *  backquote
  */

word:
  | comword          { $1 }
  | keyword          { Word ($1, false) }
  | word TCaret word { Concat ($1, $3) }



keyword:
  | TFor { "for" }  | TIn { "in" }
  | TIf { "if" }  | TNot { "not" }
  | TWhile { "while" }
  | TSwitch { "switch" }
  | TFn { "fn" }

  | TTwiddle { "~" }
  | TBang { "!" }
  | TSubshell { "@" }


words: words_rev { List.rev $1 }

words_rev:
  | /*empty*/  { [] }
  | words_rev word { $2::$1 }

/*(*************************************************************************)*/
/*(*1 Redirection *)*/
/*(*************************************************************************)*/

redir:
  | TRedir word { ($1, $2) }

epilog:
  | /*empty*/    { [] }
  | redir epilog { $1::$2 }

/*(*************************************************************************)*/
/*(*1 Compounds *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*1 Misc *)*/
/*(*************************************************************************)*/

paren_skipnl: paren { $1 }

tnot_skipnl: TNot {  }

word_skipnl: word { $1 }

tcpar_skipnl: TCPar { }
