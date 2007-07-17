%{
(*
 * parser.mly
 *
 * ocamlscheme
 *
 * Copyright (C) 2001-2004 Mark Probst
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
%}

%token <int> INT
%token <string> SYMBOL
%token LPAREN RPAREN DOT QUOTE
%start expr
%type <Type.lisp_expr> expr

%%

expr:
	  INT			{ Type.Int($1) }
	| SYMBOL		{ Type.Symbol(Symtab.intern $1) }
	| LPAREN list RPAREN	{ $2 }
	| QUOTE expr		{ Type.Cons(Type.Symbol(Symtab.intern "quote"), Type.Cons($2, Type.Nil)) }
	;

list:
	  expr DOT expr		{ Type.Cons($1, $3) }
	| expr list		{ Type.Cons($1, $2) }
	|			{ Type.Nil }
	;
