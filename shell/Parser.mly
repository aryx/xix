/*(*s: Parser.mly *)*/
%{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common
open Either
open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
/*(*s: function [[Parser.mk_seq]] *)*/
(* This is a useful optimisation as you can get lots of EmptyCommand,
 * for instance, in {\nls\n} you will get 2 EmptyCommand (for each \n)
*)
let mk_Seq (a, b) =
  match a, b with
  | EmptyCommand, _ -> b
  | _, [EmptyCommand] -> [a]
  | _ -> a::b
/*(*e: function [[Parser.mk_seq]] *)*/
%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/
/*(*s: Parser tokens *)*/
/*(*-----------------------------------------*)*/
/*(*2 Word *)*/
/*(*-----------------------------------------*)*/
%token<string * bool(* quoted *)> TWord

/*(*-----------------------------------------*)*/
/*(*2 Operators *)*/
/*(*-----------------------------------------*)*/
%token TPipe
%token<Ast.redirection_kind> TRedir
%token TAndAnd TOrOr TBang
%token TTwiddle  
%token TSemicolon 
%token TAnd
/*(*s: Parser tokens, operators other cases *)*/
%token TSubshell
/*(*x: Parser tokens, operators other cases *)*/
%token<Ast.redirection_kind * int * int> TDup
/*(*e: Parser tokens, operators other cases *)*/

/*(*-----------------------------------------*)*/
/*(*2 Variables *)*/
/*(*-----------------------------------------*)*/
%token TEq
%token TDollar
/*(*s: Parser tokens, variables other cases *)*/
%token TCount
/*(*x: Parser tokens, variables other cases *)*/
%token TStringify
/*(*e: Parser tokens, variables other cases *)*/

/*(*-----------------------------------------*)*/
/*(*2 Punctuation *)*/
/*(*-----------------------------------------*)*/
%token TOPar TCPar
%token TOBrace TCBrace
/*(*s: Parser tokens, punctuation other cases *)*/
%token TBackquote
/*(*x: Parser tokens, punctuation other cases *)*/
%token TSub
/*(*x: Parser tokens, punctuation other cases *)*/
%token TCaret
/*(*e: Parser tokens, punctuation other cases *)*/

/*(*-----------------------------------------*)*/
/*(*2 Keywords *)*/
/*(*-----------------------------------------*)*/
%token TIf TNot TWhile TSwitch TFor TIn TFn

/*(*-----------------------------------------*)*/
/*(*2 Misc *)*/
/*(*-----------------------------------------*)*/
%token TNewline

%token EOF
/*(*e: Parser tokens *)*/

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/
/*(*s: Parser token priorities *)*/
/*(* from low to high *)*/
%left TIf TWhile TFor TSwitch TCPar TNot
%left TAndAnd TOrOr
%left TBang TSubshell
%left TPipe
%left TCaret
/*(* $$A -> ($ ($ A)) not (($ $) A) *)*/
%right TDollar TCount TStringify
%left TSub
/*(*e: Parser token priorities *)*/

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/
/*(*s: Parser entry points types *)*/
%type <Ast.line> rc
/*(*s: Parser other rule types *)*/
%type <Ast.cmd> cmd
/*(*x: Parser other rule types *)*/
%type <value * value list * (redirection_kind * value, redirection_kind * int * int) Either.t list> simple
/*(*x: Parser other rule types *)*/
%type <Ast.value> first
/*(*e: Parser other rule types *)*/
%start rc
/*(*e: Parser entry points types *)*/

%%

/*(*s: grammar *)*/
/*(*************************************************************************)*/
/*(*1 line *)*/
/*(*************************************************************************)*/
rc:
  | line TNewline { Some $1 }
  | EOF           { None }

/*(*s: rule [[Parser.line]] *)*/
/*(* =~ stmt *)*/
line:
  | cmd { [$1] }
  /*(*s: [[Parser.line]] other cases *)*/
  | cmdsa line { $1 $2  }
  /*(*e: [[Parser.line]] other cases *)*/
/*(*e: rule [[Parser.line]] *)*/
/*(*s: other line related rules *)*/
cmdsa:
  | cmd TSemicolon { (fun x -> mk_Seq ($1, x)) }
  | cmd TAnd       { (fun x -> mk_Seq (Async $1, x)) }
/*(*e: other line related rules *)*/

/*(*************************************************************************)*/
/*(*1 Command *)*/
/*(*************************************************************************)*/

/*(*s: rule [[Parser.cmd]] *)*/
/*(* =~ expr *)*/
cmd:
  | /*empty*/ { EmptyCommand }
  | simple    { 
      let (cmd, args, redirs) = $1 in
      let args = List.rev args in
      let base = Simple (cmd, args) in
      /*(*s: [[Parser.cmd]] adjust [[base]] with [[redis]] *)*/
      let redirs = List.rev redirs in
      redirs |> List.fold_left (fun acc e ->
        match e with
        | Left (kind, word)    -> Redir (acc, (kind, word))
        | Right (kind, fd0, fd1) -> Dup (acc, kind, fd0, fd1)
      ) base
      /*(*e: [[Parser.cmd]] adjust [[base]] with [[redis]] *)*/
    }
  /*(*s: [[Parser.cmd]] other cases *)*/
  | cmd TAndAnd cmd { And ($1, $3) }
  | cmd TOrOr cmd   { Or  ($1, $3) }
  | TBang cmd       { Not $2 }
  /*(*x: [[Parser.cmd]] other cases *)*/
  | TTwiddle word words { Match ($2, $3) }
  /*(*x: [[Parser.cmd]] other cases *)*/
  | cmd TPipe cmd  { Pipe ($1, $3) }
  /*(*x: [[Parser.cmd]] other cases *)*/
  | TIf paren_skipnl cmd { If ($2, $3) }
  | TIf tnot_skipnl  cmd { IfNot $3 }
  /*(*x: [[Parser.cmd]] other cases *)*/
  | TWhile paren_skipnl cmd   { While ($2, $3) }
  /*(*x: [[Parser.cmd]] other cases *)*/
  | TSwitch word_skipnl brace { Switch ($2, $3) }
  /*(*x: [[Parser.cmd]] other cases *)*/
  /*(* I added the %prec TFor. *)*/
  | TFor TOPar word TIn words tcpar_skipnl cmd %prec TFor { ForIn ($3, $5, $7) }
  | TFor TOPar word tcpar_skipnl cmd           %prec TFor { For ($3, $5) }
  /*(*x: [[Parser.cmd]] other cases *)*/
  | brace epilog   { 
      let cmd = (Compound $1) in
      /*(*s: [[Parser.cmd]] in [[brace]] case, adjust [[cmd]] with [[epilog]] *)*/
      $2 |> List.fold_left (fun acc e -> 
        match e with
        | Left (kind, word) -> Redir (acc, (kind, word))
        | Right (kind, fd0, fd1) -> Dup (acc, kind, fd0, fd1)
      )  cmd
      /*(*e: [[Parser.cmd]] in [[brace]] case, adjust [[cmd]] with [[epilog]] *)*/
  }
  /*(*x: [[Parser.cmd]] other cases *)*/
  /*(* stricter: allow only word, not words *)*/
  | TFn word brace { Fn ($2, $3) }
  /*(*x: [[Parser.cmd]] other cases *)*/
  | assign cmd %prec TBang { $1 $2 }
  /*(*x: [[Parser.cmd]] other cases *)*/
  | TFn word       { DelFn $2 }
  /*(*e: [[Parser.cmd]] other cases *)*/
/*(*e: rule [[Parser.cmd]] *)*/
/*(*s: rule [[Parser.simple]] *)*/
/*(* =~ primary expr *)*/
simple:
  | first        { $1, [], [] }
  | simple word  { let (cmd, args, redirs) = $1 in (cmd, $2::args, redirs) }
  /*(*s: [[Parser.simple]] other cases *)*/
  | simple redir { let (cmd, args, redirs) = $1 in (cmd, args, $2::redirs) }
  /*(*e: [[Parser.simple]] other cases *)*/
/*(*e: rule [[Parser.simple]] *)*/
/*(*s: other command related rules *)*/
paren: TOPar body TCPar { $2 }
/*(*x: other command related rules *)*/
brace: TOBrace body TCBrace { $2 }
/*(*x: other command related rules *)*/
body: 
  | cmd         { [$1] }
  | cmdsan body { $1 $2 }
/*(*x: other command related rules *)*/
cmdsan:
  | cmdsa        { $1 }
  | cmd TNewline { (fun x -> mk_Seq ($1, x)) }
/*(*x: other command related rules *)*/
assign: first TEq word { (fun x -> Assign ($1, $3, x))  }
/*(*e: other command related rules *)*/

/*(*************************************************************************)*/
/*(*1 Word *)*/
/*(*************************************************************************)*/

/*(*s: rule [[Parser.comword]] *)*/
comword:
  | TWord                         { Word (fst $1, snd $1) }
  /*(*s: [[Parser.comword]] other cases *)*/
  | TDollar word                  { Dollar $2 }
  /*(*x: [[Parser.comword]] other cases *)*/
  | TOPar words TCPar             { List $2 }
  /*(*x: [[Parser.comword]] other cases *)*/
  | TBackquote brace              { CommandOutput $2 }
  /*(*x: [[Parser.comword]] other cases *)*/
  | TCount word                   { Count $2 }
  | TDollar word TSub words TCPar { Index ($2, $4) }
  | TStringify word               { Stringify $2 }
  /*(*e: [[Parser.comword]] other cases *)*/
/*(*e: rule [[Parser.comword]] *)*/
/*(*s: rule [[Parser.word]] *)*/
word:
  | comword          { $1 }
  | keyword          { Word ($1, false) }
  /*(*s: [[Parser.word]] other cases *)*/
  | word TCaret word { Concat ($1, $3) }
  /*(*e: [[Parser.word]] other cases *)*/
/*(*e: rule [[Parser.word]] *)*/
/*(*s: other word related rules *)*/
first:
  | comword           { $1 }
  /*(*s: [[Parser.first]] other cases *)*/
  | first TCaret word { Concat ($1, $3) }
  /*(*e: [[Parser.first]] other cases *)*/
/*(*x: other word related rules *)*/
keyword:
  | TFor { "for" }  | TIn { "in" }
  | TIf { "if" }  | TNot { "not" }
  | TWhile { "while" }
  | TSwitch { "switch" }
  | TFn { "fn" }
  /*(*s: [[Parser.keyword]] other cases *)*/
  | TBang { "!" }
  /*(*x: [[Parser.keyword]] other cases *)*/
  | TTwiddle { "~" }
  /*(*x: [[Parser.keyword]] other cases *)*/
  | TSubshell { "@" }
  /*(*e: [[Parser.keyword]] other cases *)*/
/*(*x: other word related rules *)*/
words: words_rev { List.rev $1 }
/*(*x: other word related rules *)*/
words_rev:
  | /*empty*/  { [] }
  | words_rev word { $2::$1 }
/*(*e: other word related rules *)*/

/*(*************************************************************************)*/
/*(*1 Redirection *)*/
/*(*************************************************************************)*/

/*(*s: rule [[Parser.redir]] *)*/
redir:
  | TRedir word { Left ($1, $2) }
  /*(*s: [[Parser.redir]] other cases *)*/
  | TDup        { Right $1 }
  /*(*e: [[Parser.redir]] other cases *)*/
/*(*e: rule [[Parser.redir]] *)*/
/*(*s: other redirection related rules *)*/
epilog:
  | /*empty*/    { [] }
  | redir epilog { $1::$2 }
/*(*e: other redirection related rules *)*/

/*(*************************************************************************)*/
/*(*1 Skipnl hacks *)*/
/*(*************************************************************************)*/

/*(*s: skipnl rules *)*/
paren_skipnl: paren { Globals.skipnl := true; $1 }
tnot_skipnl: TNot   { Globals.skipnl := true; }
/*(*x: skipnl rules *)*/
word_skipnl: word   { Globals.skipnl := true; $1 }
tcpar_skipnl: TCPar { Globals.skipnl := true; }
/*(*e: skipnl rules *)*/
/*(*e: grammar *)*/
/*(*e: Parser.mly *)*/
