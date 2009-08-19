FILES=rdf-util.ss rdf2html.ss web-app.ss rdf-print.ss web-gen.ss srdf.ss \
    server.ss
NW_FILES=rdf-util.nw rdf2html.nw web-app.nw rdf-print.nw web-gen.nw srdf.nw \
    server.nw

.SUFFIXES: .so .ss .sls .nw

build: ${FILES}

www: web-gen.ss
	build-pages.ss

www_sacrideo: web-gen.ss
	cp web-param.ss web-param.ss.stock
	patch web-param.ss web-param-sacrideo.patch
	build-pages.ss
	mv web-param.ss.stock web-param.ss

sacrideo: build
	cp web-param.ss web-param.ss.stock
	patch web-param.ss web-param-sacrideo.patch
	@echo '(compile-file "web-app")' | scheme -q
	mv web-param.ss.stock web-param.ss

docs: 
	noweave -delay -t2 ${NW_FILES} > doc/tech/main.tex

clean: 
	rm -f ${FILES} ${WEB_FILES}
	rm -f *.so

clean_www:
	rm -rf www_static

.nw.ss: 
	notangle -R$@ -t2 $< > $@

.nw.sls:
	notangle -R$@ -t2 $< > $@

.ss.so:
	@echo '(compile-file "$*")' | scheme -q
