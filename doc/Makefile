.SUFFIXES: .pdf .ps .dvi .tex

.tex.dvi:
	tex $<

.dvi.ps:
	dvips -o $@ $<

.ps.pdf:
	ps2pdf $<
