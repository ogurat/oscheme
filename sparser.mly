%{
open Syntax
%}

%token LPAREN RPAREN QUOTE DOT QQUOTE COMMA COMMAAT
%token EOF
%token SHARPSEMICOLON
%token SHARPLPAREN
%token <int> INTV
%token <bool> BOOLV
%token <char> CHARV
%token <string> ID
%token <string> STRINGV
%token <string> COMMENT


%start toplevel sexpdata
%type <Syntax.sexp list> toplevel
%type <Syntax.sexp> sexpdata
%%

sexpdata : Sexp {$1}

toplevel :
|  Sexplist EOF { $1 }


/*
toplevel :
    Sexp { Prog $1 }
*/
Sexp :
    INTV { Int $1 }
  | CHARV { Char $1 }
  | BOOLV { Bool $1 }
  | ID { Id $1 }
  | STRINGV { String $1 }
  | LPAREN Sexplist RPAREN { List $2 }
  | LPAREN Dotpair RPAREN { $2 }
  | SHARPLPAREN Sexplist RPAREN { Vector $2 }
  | QUOTE Sexp { List [Id "quote"; $2]}
  | QQUOTE Sexp { List [Id "quasiquote"; $2] }
  | COMMA Sexp { List [Id "unquote"; $2] }
  | COMMAAT Sexp { List [Id "unquote-splicing"; $2] }
Sexplist :
 /* empty */ { [] }
  | Sexp Sexplist { $1 :: $2 }
  | SHARPSEMICOLON Sexp Sexplist { $3 }
Dotpair :
  | Sexp DOT Sexp  {
           match $3 with
           | List x -> List ($1 :: x)
           | _ -> Cons ($1, $3)
	 }
  | Sexp Dotpair {
           match $2 with
             List x -> List ($1 :: x)
           | _ -> Cons ($1, $2)
	 }
/*
  | Sexp DOT Sexp  { Cons ($1, $3) }
  | Sexp Dotpair { Cons ($1, $2) }
*/

/*
SexpComment :
    SHARPSEMICOLON Sexp Sexp { $3 }
  | Sexp SHARPSEMICOLON Sexp { $1 }

SC :
    SHARPSEMICOLON Sexp {  }
*/
