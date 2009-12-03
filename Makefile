FILES=extended-definitions.sls modlisp.sls sockets.sls swap.sls base64.sls \
	ffi-bind.sls parser-util.sls rfc2822.sls mime.sls errno.sls \
        ffi-bind/types.chezscheme.sls

DOC_DIR=doc/

all: ffi-bind-param ${FILES}

ffi-bind-param:
	mkdir -p ffi-bind
	notangle -R'ffi-bind/parameters.sls' -t2 ffi-bind.nw > ffi-bind/parameters.sls

include noweb_make.mk
