thesis: $(wildcard *.tex)
	latexmk -lualatex -pdf main.tex

watch:
	ls *.tex | entr -s "make thesis"

clean:
	latexmk -c
