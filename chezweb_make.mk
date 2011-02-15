.SUFFIXES: .pdf .ps .dvi .tex .w

.w.tex:
	chezweave $<

.tex.pdf:
	xetex $<
	xetex $<
