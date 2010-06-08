.SUFFIXES: .pdf .ps .dvi .tex .w

.w.tex:
	chezweave $<

.tex.dvi:
	tex $<
	tex $<

.dvi.ps:
	dvips -o $*.ps -t letter $<

.dvi.pdf:
	dvipdfm -o $*.pdf -p letter $<
