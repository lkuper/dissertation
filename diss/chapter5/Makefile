PAPER=main
LATEX=pdflatex
UNAME := $(shell uname)

default: quick

all: paper

#-------------------------------------------------------------------
# These targets just build the paper without changing the white/grey
# editing-markup/nomarkup state:

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

# A little shorthand for Ryan:
white:
	rm -f activategreybg.tex
	${MAKE} paper

grey:
	touch editingmarks.tex
	touch activategreybg.tex
	${MAKE} paper

ed: editingmarks
editingmarks:
	touch editingmarks.tex
	${MAKE} paper
#	rm editingmarks.tex

plain: 
	rm -f editingmarks.tex
	${MAKE} paper
	cp ${PAPER}.pdf plain.pdf

#-----------------------------------------------
# Others:

# Check style (due to
# http://matt.might.net/articles/shell-scripts-for-passive-voice-weasel-words-duplicates/):
style:
	echo "weasel words: "
	sh ../bin/weasel *.tex
	echo
	echo "passive voice: "
	sh ../bin/passive *.tex
	echo
	echo "duplicates: "
	perl ../bin/dups *.tex

open: ${PAPER}.pdf
ifeq (${UNAME}, Darwin)
	open ${PAPER}.pdf
endif
ifeq (${UNAME}, Linux)
	evince ${PAPER}.pdf &
endif

clean: 
	rm -f *.out *.aux *.bbl *.log *.blg


