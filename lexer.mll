

{

}



let specialinitial = ['!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']
let initial = ['a'-'z' '!' '$' '%'  '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~']


rule main = parse

| ';'  [^ '\r' '\n' '\012']* { main lexbuf }
  (* ignore spacing and newline characters *)
|  [' ' '\009' '\012' '\r' '\n']+   { main lexbuf }


| '"'      { string_lit (Buffer.create 100) lexbuf }
(*
| '"' [^ '"']* '"' { Sparser.STRINGV (Lexing.lexeme lexbuf) }
 *)

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

and string_lit buf = parse

  | '"'       { Sparser.STRINGV (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; string_lit buf lexbuf }
  | '\\' 'a'  { Buffer.add_char buf '\007'; string_lit buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; string_lit buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; string_lit buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; string_lit buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; string_lit buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"'; string_lit buf lexbuf }
  | '\\' '|'  { Buffer.add_char buf '|'; string_lit buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      string_lit buf lexbuf
    }

