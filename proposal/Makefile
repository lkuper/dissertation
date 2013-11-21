PAPER=thesis-proposal
LATEX=pdflatex
UNAME := $(shell uname)

default: quick

all: paper

#---------------------------------------------------------------------
# These targets just build the paper without changing the build state:

quick:
	${LATEX} ${PAPER}.tex
	${MAKE} open

paper:
	${LATEX} ${PAPER}.tex
	bibtex ${PAPER}
	${LATEX} ${PAPER}.tex
	bibtex ${PAPER}
	${LATEX} ${PAPER}.tex
	${MAKE} open

#-----------------------------------------------
# These targets actually change the build state:

ed: editingmarks

editingmarks:
	touch editingmarks.tex
	${MAKE} paper

plain: 
	rm -f editingmarks.tex
	${MAKE} paper
	cp ${PAPER}.pdf plain.pdf

#-----------------------------------------------
# Others:

open: ${PAPER}.pdf
ifeq (${UNAME}, Darwin)
	open ${PAPER}.pdf
endif
ifeq (${UNAME}, Linux)
	evince ${PAPER}.pdf &
endif

clean: 
	rm -f *.out *.aux *.bbl *.log *.blg


