(*
 * lexer.mll
 *
 * ocamlscheme
 *
 * Copyright (C) 2001-2007 Mark Probst
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)

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
  | ';'[^'\n']*		      { token lexbuf }
  | [^'(' ')' '0' - '9' ' ' '\t' '\n' '.' '\''][^' ' '\t' '\n' '(' ')']*    { SYMBOL(Lexing.lexeme lexbuf) }
  | eof                       { raise Eof }
