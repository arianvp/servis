all: paper.pdf


paper.pdf: paper.tex
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode -shell-escape"  -use-make paper.tex

clean:
	latexmk -CA
	rm -rf _minted-paper
