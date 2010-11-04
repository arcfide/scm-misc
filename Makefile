FILES=extended-definitions.sls modlisp.sls sockets.sls swap.sls base64.sls \
	ffi-bind.sls parser-util.sls rfc2822.sls mime.sls errno.sls \
        sockets/compat.chezscheme.sls

DOC_DIR=doc/

all: ${FILES}

.c.so: 
	cc -shared -fPIC -o $*.so.1 $<

include noweb_make.mk
include chezweb_make.mk
