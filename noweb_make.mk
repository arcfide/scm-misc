.SUFFIXES: .so .sls .ss .scm .tex .nw

.nw.sls:
	notangle -R$(@F) $< > $@
.nw.ss:
	notangle -R$(@F) $< > $@
.nw.scm:
	notangle -R$(@F) $< > $@

.nw.tex:
	noweave -tex -t2 $< > ${DOC_DIR}$(subst /,_,$(subst ${ROOT_DIR},,$@))
