

{

}



let specialinitial = ['!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']
let initial = ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']


rule main = parse

| ';'  [^ '\r' '\n']* { main lexbuf }
  (* ignore spacing and newline characters *)
|  [' ' '\009' '\012' '\r' '\n']+   { main lexbuf }


| '"' [^ '"']* '"' { Sparser.STRINGV (Lexing.lexeme lexbuf) }

| ['0'-'9']+
    { Sparser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
| '\'' { Sparser.QUOTE }
| "(" { Sparser.LPAREN }
| ")" { Sparser.RPAREN }

  
| ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']
   ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~' '0'-'9' '+' '-' '.' '@' '\'']*
| '+'
| '-'
| "..."
    { let id = Lexing.lexeme lexbuf in
      Sparser.ID id
    }
| "#t" { Sparser.BOOLV true } | "#f" { Sparser.BOOLV false }

| "#;" { Sparser.SHARPSEMICOLON }

| eof { Sparser.EOF }


