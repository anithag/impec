MAIN=impec

OBJS = ast.cmo lexer.cmo parser.cmo cast.cmo helper.cmo ctranslator.cmo cappfuncprinter.cmo cprinter.cmo main.cmo

%.cmo : %.ml
	ocamlc -g -c $<

%.cmi : %.mli
	ocamlc -g -c $<


$(MAIN): clean $(OBJS)
	ocamlc -g -o $(MAIN) unix.cma str.cma $(OBJS) 

lexer.ml : lexer.mll
	ocamllex -q $<

lexer.cmo : parser.cmi lexer.ml
	ocamlc -g -c lexer.ml

parser.ml : parser.mly
	ocamlyacc -q $<

parser.mli : parser.mly
	ocamlyacc -q $<

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(MAIN) app.c Enclave.c
