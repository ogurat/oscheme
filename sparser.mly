%{
open Syntax
%}

%token LPAREN RPAREN QUOTE
%token EOF
%token SHARPSEMICOLON
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
  | QUOTE Sexp { List [Id "quote"; $2]}
Sexplist :
 /* empty */ { [] }
  | Sexp Sexplist { $1 :: $2 }
  | SHARPSEMICOLON Sexp Sexplist { $3 }

/*
SexpComment :
    SHARPSEMICOLON Sexp Sexp { $3 }
  | Sexp SHARPSEMICOLON Sexp { $1 }

SC :
    SHARPSEMICOLON Sexp {  }
*/
