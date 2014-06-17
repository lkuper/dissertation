NAME=dissertation
UNAME := $(shell uname)

$(NAME).pdf: *.tex *.bib
	pdflatex $(NAME).tex
	bibtex $(NAME)
	pdflatex $(NAME).tex
	pdflatex $(NAME).tex
	${MAKE} open

open: ${NAME}.pdf
ifeq (${UNAME}, Darwin)
	open ${NAME}.pdf
endif
ifeq (${UNAME}, Linux)
	evince ${NAME}.pdf &
endif

clean:
	rm -f *.blg *.bbl *.aux *.log $(NAME).pdf *.idx *.out *.toc
