%{
%}

%token PLUS MULT TOPAR TCPAR
%token <string> ID

%start e

%%

e: e PLUS t { }
 | t { }
 ;

t: t MULT f { }
 | f { }
 ;

f: TOPAR e TCPAR { }
 | ID { }
 ;


