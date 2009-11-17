.SUFFIXES: .so .sls .ss .scm .tex .nw

.nw.sls:
	notangle -R$(@F) -t2 $< > $@
.nw.ss:
	notangle -R$(@F) -t2 $< > $@
.nw.scm:
	notangle -R$(@F) -t2 $< > $@

.nw.tex:
	noweave -tex -t2 $< > ${DOC_DIR}$(subst /,_,$(subst ${ROOT_DIR},,$@))
