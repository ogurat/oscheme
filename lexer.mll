

{
let reservedWords = [
  (* Keywords *)
(*
  ("+", Sparser.PLUS);
  ("-", Sparser.MINUS);
  ("*", Sparser.MUL);
  ("add1", Sparser.ADD1);
  ("sub1", Sparser.SUB1);
  ("=", Sparser.EQ);
  ("<", Sparser.LT);
  ("if", Sparser.IF);
  ("and", Sparser.AND);
  ("or", Sparser.OR);
  ("not", Sparser.NOT);
  ("let", Sparser.LET);
  ("lambda", Sparser.LAMBDA);
  ("letrec", Sparser.LETREC);
  ("set", Sparser.SET);
  ("begin", Sparser.BEGIN);
  ("setcar", Sparser.SETCAR);
  ("setcdr", Sparser.SETCDR); *)
                    ] 
  (* 構文キーワード  *)
  let keywords = [
    "else";
    "=>";
    "define";
    "unquote";
    "unquote-splicing";
    "quote";
    "lambda";
    "if";
    "set!";
    "begin";
    "cond";
    "and";
    "or";
    "case";
    "let";
    "let*";
    "letrec";
    "do";
    "delay";
     "quasiquote"]
}



let specialinitial = ['!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']
let initial = ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']


rule main = parse

| ';' [^ '\r' '\n']* { main lexbuf }
  (* ignore spacing and newline characters *)
|  [' ' '\009' '\012' '\r' '\n']+   { main lexbuf }


| '"' [^ '"']* '"' { Sparser.STRINGV (Lexing.lexeme lexbuf) }

| ['0'-'9']+
    { Sparser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
| '\'' { Sparser.QUOTE }
| "(" { Sparser.LPAREN }
| ")" { Sparser.RPAREN }


(*
| "+" { Sparser.ID "+"}
| "-" { Sparser.ID "-"}
| "..." { Sparser.ID "..."}
*)
  
| ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']
   ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~' '0'-'9' '+' '-' '.' '@' '\'']*
| '+'
| '-'
| "..."
    { let id = Lexing.lexeme lexbuf in
      (*try 
        List.assoc id reservedWords
      with
      _ ->*) Sparser.ID id
    }
| "#t" { Sparser.BOOLV true } | "#f" { Sparser.BOOLV false }
| eof { Sparser.EOF }


