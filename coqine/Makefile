OCAMLC=ocamlc
OCAMLOPT=ocamlopt

COQSRC=..

MLDIRS=-I $(COQSRC) -I $(COQSRC)/config -I $(COQSRC)/lib -I $(COQSRC)/kernel -I +camlp5
BYTEFLAGS=$(MLDIRS) -pp camlp5o -g
OPTFLAGS=$(MLDIRS) -pp camlp5o

EXENAME=coqine

MCHECKERLOCAL :=\
  declarations.cmo environ.cmo \
  closure.cmo reduction.cmo \
  type_errors.cmo \
  modops.cmo \
  inductive.cmo typeops.cmo \
  indtypes.cmo subtyping.cmo mod_checking.cmo \
  safe_typing.cmo check.cmo \
  check_stat.cmo checker.cmo coqine.cmo main.cmo

MCHECKER:=\
  $(COQSRC)/config/coq_config.cmo \
  $(COQSRC)/lib/pp_control.cmo $(COQSRC)/lib/pp.cmo $(COQSRC)/lib/compat.cmo \
  $(COQSRC)/lib/flags.cmo $(COQSRC)/lib/util.cmo \
  $(COQSRC)/lib/option.cmo $(COQSRC)/lib/hashcons.cmo \
  $(COQSRC)/lib/system.cmo \
  $(COQSRC)/lib/predicate.cmo $(COQSRC)/lib/rtree.cmo \
  $(COQSRC)/kernel/names.cmo $(COQSRC)/kernel/univ.cmo \
  $(COQSRC)/kernel/envars.cmo \
  validate.cmo \
  $(COQSRC)/kernel/esubst.cmo term.cmo \
  $(MCHECKERLOCAL)

MCHECKEROPT=$(MCHECKER:.cmo=.cmx)

all: byte

byte : $(EXENAME)$(EXE)
opt : $(EXENAME).opt$(EXE)

$(EXENAME)$(EXE): main.cmo coqine.cmo
	ocamlc $(BYTEFLAGS) -o $@ unix.cma gramlib.cma $(MCHECKER)

$(EXENAME).opt$(EXE): main.cmx coqine.cmx
	ocamlopt $(OPTFLAGS) -o $@ unix.cmxa gramlib.cmxa $(MCHECKEROPT)

test: test.vo
	./$(EXENAME)$(EXE) $<

test.vo: test.v
	coqc -nois $<

.SUFFIXES:.ml .mli .cmi .cmo .cmx

.ml.cmo:
	$(OCAMLC) -c $(BYTEFLAGS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(OPTFLAGS) $<

.mli.cmi:
	$(OCAMLC) -c $(BYTEFLAGS) $<


depend::
	ocamldep $(MLDIRS) -pp camlp5o config/*.{ml,mli} kernel/*.{ml,mli} lib/*.{ml,mli} *.{ml,mli} > .depend

clean::
	rm -f *.cm* *.o *.a *~ $(BINARIES)

-include .depend
