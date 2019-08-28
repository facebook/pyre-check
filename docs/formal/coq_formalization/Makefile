# Makefile originally taken from coq-club

%: Makefile.coq phony
	+make -f Makefile.coq $@

all: Makefile.coq
	+make -f Makefile.coq all

clean: Makefile.coq
	+make -f Makefile.coq clean
	rm -f Makefile.coq
	rm -f CoqMakefile.conf
	rm -rf html
	find . -name "*.aux" -delete
	find . -name "*.glob" -delete

Makefile.coq: _CoqProject Makefile
	coq_makefile -f _CoqProject | sed 's/$$(COQCHK) $$(COQCHKFLAGS) $$(COQLIBS)/$$(COQCHK) $$(COQCHKFLAGS) $$(subst -Q,-R,$$(COQLIBS))/' > Makefile.coq

doc: all
	mkdir -p html
	coqdoc -toc -g -utf8 -html -d html *.v

_CoqProject: ;

Makefile: ;

phony: ;

.PHONY: all clean phony
