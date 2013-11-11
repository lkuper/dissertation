PAPER=lindsey-kuper-thesis-proposal
LATEX=pdflatex
UNAME := $(shell uname)

default: quick

all: paper

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

open: ${PAPER}.pdf
ifeq (${UNAME}, Darwin)
	open ${PAPER}.pdf
endif
ifeq (${UNAME}, Linux)
	evince ${PAPER}.pdf &
endif

clean: 
	rm -f *.out *.aux *.bbl *.log *.blg


