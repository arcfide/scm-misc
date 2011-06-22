.SUFFIXES: .pdf .ps .dvi .tex .w

.w.tex:
	chezweave $<

.tex.pdf:
	xetex -papersize=letter $<
	xetex -papersize=letter $<
