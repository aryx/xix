# The lexer generator

COMPFLAGS=
LINKFLAGS=
CAMLC=../boot/ocamlrun ../boot/ocamlc -I ../boot
YACCFLAGS=
CAMLYACC=../boot/ocamlyacc
CAMLLEX=../boot/ocamlrun ../boot/ocamllex
DEPFLAGS=
CAMLDEP=../boot/ocamlrun ../tools/misc/ocamldep

OBJS=ast.cmo parser.cmo lexer.cmo lexgen.cmo compact.cmo output.cmo main.cmo

#pad: if want to compile with a more recent ocaml and use a recent ocamldebug
#CAMLLEX=ocamllex
#CAMLYACC=ocamlyacc
#CAMLC=ocamlc -g -I ../yacc2/compat
#OBJS=../yacc2/compat/set.cmo ../yacc2/compat/map.cmo parser.cmo lexer.cmo lexgen.cmo compact.cmo output.cmo main.cmo

all: ocamllex

ocamllex: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamllex $(OBJS)

clean::
	rm -f ocamllex
	rm -f *.cmo *.cmi

parser.ml parser.mli: parser.mly
	$(CAMLYACC) $(YACCFLAGS) parser.mly
	perl -p -i -e 's#/\*\(\*[sex]: .* \*\)\*/##' parser.ml
#pad: this  perl thing is just because ocamlyacc does not handle well
# syncweb special comment at the very end

clean::
	rm -f parser.ml parser.mli

beforedepend:: parser.ml parser.mli

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

clean::
	rm -f lexer.ml

beforedepend:: lexer.ml

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
