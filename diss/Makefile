NAME=dissertation

$(NAME).pdf: *.tex *.bib
	pdflatex $(NAME).tex
	bibtex $(NAME)
	pdflatex $(NAME).tex
	pdflatex $(NAME).tex

clean:
	rm -f *.blg *.bbl *.aux *.log $(NAME).pdf *.idx *.out *.toc
