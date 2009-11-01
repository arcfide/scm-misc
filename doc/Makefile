.SUFFIXES: .pdf .ps .dvi .tex

.tex.dvi:
	tex $<

.dvi.ps:
	dvips -o $@ $<

.dvi.pdf:
	dvipdfm -o $@ $<
