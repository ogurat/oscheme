

{

}



let specialinitial = ['!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']
let initial = ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']


rule main = parse

| ';'  [^ '\r' '\n' '\012']* { main lexbuf }
  (* ignore spacing and newline characters *)
|  [' ' '\009' '\012' '\r' '\n']+   { main lexbuf }


| '"' [^ '"']* '"' { Sparser.STRINGV (Lexing.lexeme lexbuf) }

| '-'? ['0'-'9']+
    { Sparser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
| '\'' { Sparser.QUOTE }
| '(' { Sparser.LPAREN }
| ')' { Sparser.RPAREN }
| '.' { Sparser.DOT }
| '`' { Sparser.QQUOTE }
| ",@" { Sparser.COMMAAT }
| ',' { Sparser.COMMA }
  
| ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~' '+' '-']
   ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~' '0'-'9' '+' '-' '.' '@' '\'']*
| "..."
    { let id = Lexing.lexeme lexbuf in
      Sparser.ID id
    }
| "#t" { Sparser.BOOLV true } | "#f" { Sparser.BOOLV false }

| "#;" { Sparser.SHARPSEMICOLON }

| "#(" { Sparser.SHARPLPAREN }
| '#' '\\' ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~' '0'-'9' '+' '-' '.' '@' '\''] { Sparser.CHARV (Lexing.lexeme_char lexbuf 2) }

| eof { Sparser.EOF }

| _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }
