OC = ocamlopt
OBJS = symtab.cmx parser.cmx lexer.cmx lisp.cmx

all : lisp

lisp : $(OBJS)
	$(OC) -o lisp $(OBJS)

symtab.cmo symtab.cmx : type.cmi

lisp.cmo lisp.cmx : parser.cmi lexer.cmo symtab.cmi type.cmi

parser.cmi parser.cmo parser.cmx : symtab.cmi type.cmi

parser.cmx : parser.cmi

lexer.cmo lexer.cmx : type.cmi

%.cmi : %.mli
	$(OC) -c $<

%.cmo : %.ml
	$(OC) -c $<

%.cmx : %.ml
	$(OC) -c $<

%.mli : %.ml
	$(OC) -i $< >$@

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

lexer.ml : lexer.mll
	ocamllex lexer.mll

clean :
	rm -f *~ core *.cmi *.cmo *.cmx *.o *.mli lexer.ml parser.ml parser.mli lisp
