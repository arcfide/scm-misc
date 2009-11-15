FILES=extended-definitions.sls modlisp.sls sockets.sls swap.sls base64.sls ffi-bind.sls \
	parser-util.sls rfc2822.sls mime.sls errno.sls
        
ROOT_DIR=/home/arcfide/code/arcfide/

.SUFFIXES: .so .sls .ss .scm .tex .nw

all: ffi-bind-param ${FILES}

ffi-bind-param:
	mkdir -p ffi-bind
	notangle -R'ffi-bind/parameters.sls' -t2 ffi-bind.nw > ffi-bind/parameters.sls

.nw.sls:
	notangle -R$(@F) -t2 $< > $@
.nw.ss:
	notangle -R$(@F) -t2 $< > $@
.nw.scm:
	notangle -R$(@F) -t2 $< > $@

.nw.tex:
	mkdir -p doc
	noweave -tex -t2 $< > doc/$(subst /,_,$(subst ${ROOT_DIR},,$@))
