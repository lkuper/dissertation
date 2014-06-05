NAME=dissertation

$(NAME).pdf: *.tex *.bib is-abbrv.bst
	pdflatex $(NAME).tex
	bibtex $(NAME)
	pdflatex $(NAME).tex
	pdflatex $(NAME).tex


is-abbrv.bst:
	wget http://ftp.math.utah.edu/pub/tex/bibtex/is-abbrv.bst

clean:
	rm -f *.blg *.bbl *.aux *.log $(NAME).pdf *.idx *.out *.toc
