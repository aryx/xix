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
%token<Ast.redirection> TRedir
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
  | EOF           { }
  | line TNewline { }

/*(* =~ stmt *)*/
line:
  | cmd { }
  | cmdsa line { }

cmdsa:
  | cmd TSemicolon { }
  | cmd TAnd { }

/*(*************************************************************************)*/
/*(*1 Command *)*/
/*(*************************************************************************)*/

/*(* =~ expr *)*/
cmd:
  | /*empty*/ { }
  | simple    { }

  | cmd TAndAnd cmd { }
  | cmd TOrOr cmd { }
  | TBang cmd { }
  | TTwiddle word words { }

  | cmd TPipe cmd { }
  | brace epilog { }

/* why not put those with line: ?*/

  | TIf paren_skipnl cmd { }
  | TIf tnot_skipnl  cmd { }

  | TWhile paren_skipnl cmd { }
  | TSwitch word_skipnl brace { }


  | TFor TOPar word TIn words tcpar_skipnl cmd %prec TFor 
     { }
  | TFor TOPar word tcpar_skipnl cmd           %prec TFor 
     { }


  /*(* stricter: allow only word *)*/
  | TFn word brace { }
  | TFn word { }

  | assign cmd %prec TBang { }



/*(* =~ primary expr *)*/
simple:
  | first       { }
  | simple word { }
  | simple redir { }


paren: TOPar body TCPar { }

brace: TOBrace body TCBrace { }

body: 
  | cmd { }
  | cmdsan body { }

cmdsan:
  | cmdsa { }
  | cmd TNewline { }

assign: first TEq word { }


/*(*************************************************************************)*/
/*(*1 Word *)*/
/*(*************************************************************************)*/

first:
  | comword { }
  | first TCaret word { }


comword:
  | TWord { }
  | TDollar word { }
  | TCount word { }
  | TDollar word TSub words TCPar { }
  | TOPar words TCPar  { }
  /* less: TStringify
   *  backquote
  */

word:
  | comword { }
  | word TCaret word { }
  | keyword { }



keyword:
  | TFor { }  | TIn  { }
  | TIf { }  | TNot { }
  | TWhile { }
  | TSwitch { }
  | TFn { }

  | TTwiddle { }
  | TBang { }
  | TSubshell { }


words:
  | /*empty*/ { }
  | words word { }

/*(*************************************************************************)*/
/*(*1 Redirection *)*/
/*(*************************************************************************)*/

redir:
  | TRedir word { }

epilog:
  | /*empty*/ { }
  | redir epilog { }

/*(*************************************************************************)*/
/*(*1 Compounds *)*/
/*(*************************************************************************)*/

/*(*************************************************************************)*/
/*(*1 Misc *)*/
/*(*************************************************************************)*/

paren_skipnl: paren { }

tnot_skipnl: TNot { }

word_skipnl: word { }

tcpar_skipnl: TCPar { }
