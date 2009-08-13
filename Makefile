.SUFFIXES: .so .sls .ss .scm .tex .nw

.nw.sls:
	notangle -R$@ -t2 $< > $@
.nw.ss:
	notangle -R$@ -t2 $< > $@
.nw.scm:
	notangle -R$@ -t2 $< > $@

.nw.tex:
	mkdir -p doc
	noweave -tex -t2 $< > doc/$@
