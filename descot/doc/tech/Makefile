DEPS=nwmac.tex main.tex bnf.tex 
FILENAME=descot_techdoc

${FILENAME}.dvi: ${FILENAME}.tex ${DEPS}
	tex ${FILENAME}.tex

${FILENAME}.ps: ${FILENAME}.dvi 
	dvips ${FILENAME}.dvi

${FILENAME}.pdf: ${FILENAME}.ps 
	ps2pdf ${FILENAME}.ps

clean:
	rm -f ${FILENAME}.{dvi,ps,pdf}
