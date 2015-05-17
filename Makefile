# Ocaml commands
CAMLC = ocamlc
CAMLOPT = ocamlopt -inline 20
COMPILER = $(CAMLC) -c
OPTCOMP = $(CAMLOPT) -c
LINKER = $(CAMLC)
OPTLINK = $(CAMLOPT)

# Directories
OCAMLINC = 

# Files
LIB = bigarray.cma 
OPTLIB = bigarray.cmxa
OBJS = util.cmo chromosome.cmo population.cmo selection.cmo genesys_selection.cmo ant.cmo simulator.cmo genesys.cmo
OPTOBJS = $(OBJS:.cmo=.cmx)

# Default rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo: 
	$(COMPILER) $(OCAMLINC) $<

.ml.cmx:
	$(OPTCOMP) $(OCAMLINC) $<

.mli.cmi:
	$(COMPILER) $(OCAMLINC) $<

all: knapsack genesys 

all.opt: knapsack.opt genesys.opt 

knapsack: $(OBJS)
	$(LINKER) $(OCAMLINC) $(LIB) $(OBJS) -o knapsack knapsack.ml

knapsack.opt: $(OPTOBJS)
	$(OPTLINK) -o knapsack.opt $(OCAMLINC) $(OPTLIB) $(OPTOBJS) knapsack.ml

genesys: $(OBJS)
	$(LINKER) $(OCAMLINC) $(LIB) $(OBJS) -o genesys genesys_test.ml

genesys.opt: $(OPTOBJS)
	$(OPTLINK) -o genesys.opt $(OCAMLINC) $(OPTLIB) $(OPTOBJS) genesys_test.ml

clean:
	rm -f genesys genesys.opt knapsack knapsack.opt *.cm* *.o *.a

depend:
	ocamldep *.ml *.mli > .depend

#dependencies
include .depend