%{
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
