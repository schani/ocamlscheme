{
open Parser
exception Eof
}

rule token = parse
    [' ' '\t' '\n']+          { token lexbuf }
  | ['0'-'9']+                { INT(int_of_string(Lexing.lexeme lexbuf)) }
  | '('                       { LPAREN }
  | ')'                       { RPAREN }
  | '.'                       { DOT }
  | '\''                      { QUOTE }
  | [^'(' ')' '0' - '9' ' ' '\t' '\n' '.' '\''][^' ' '\t' '\n' '(' ')']*    { SYMBOL(Lexing.lexeme lexbuf) }
  | eof                       { raise Eof }
