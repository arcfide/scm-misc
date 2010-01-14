DOC_DIR=../doc/sockets_

include ../noweb_make.mk
include ../chez_make.mk

SCHEME_H= ${CHEZPREFIX}/${MACHINETYPE}/scheme.h
SOCKETS_LIB_MAJOR=1

SOCKETS_LIB=sockets.so.${SOCKETS_LIB_MAJOR}

build: ${SOCKETS_LIB}

scheme.h: ${SCHEME_H}
	ln -s ${SCHEME_H} scheme.h

${SOCKETS_LIB}: scheme.h sockets-stub.c
	cc -shared -fPIC -o ${SOCKETS_LIB} sockets-stub.c
