.SUFFIXES: .so .sls .ss .scm .tex .nw

.nw.sls:
	notangle -R$@ $< > $@
.nw.ss:
	notangle -R$@ $< > $@
.nw.scm:
	notangle -R$@ $< > $@

.nw.tex:
	mkdir -p doc
	noweave -tex -t2 $< > doc/$@
