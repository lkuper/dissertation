NAME := dissertation
LATEX := xelatex
UNAME := $(shell uname)
SOURCES := $(wildcard *.tex *.bib) $(wildcard */*.tex) $(wildcard */figures/*.pdf) $(wildcard ../latex_common/*.tex)
TEX_SOURCES := $(wildcard *.tex) $(wildcard */*.tex) $(wildcard ../latex_common/*.tex)

$(NAME).pdf: $(SOURCES)
	$(LATEX) $(NAME).tex
	bibtex $(NAME)
	$(LATEX) $(NAME).tex
	$(LATEX) $(NAME).tex

#-----------------------------------------------
# These targets actually change the build state:

ed: editingmarks

editingmarks:
	touch editingmarks.tex
	$(MAKE) $(NAME).pdf

plain: 
	rm -f editingmarks.tex
	$(MAKE) $(NAME).pdf
	cp $(NAME).pdf plain.pdf

#-----------------------------------------------
# Others:

# Check style (due to
# http://matt.might.net/articles/shell-scripts-for-passive-voice-weasel-words-duplicates/):
style:
	echo "weasel words: "
	sh ../bin/weasel $(TEX_SOURCES)
	echo
	echo "passive voice: "
	sh ../bin/passive $(TEX_SOURCES)
	echo
	echo "duplicates: "
	perl ../bin/dups $(TEX_SOURCES)

open: $(NAME).pdf
ifeq ($(UNAME), Darwin)
	open $(NAME).pdf
endif
ifeq ($(UNAME), Linux)
	evince $(NAME).pdf &
endif

clean:
	rm -f *.blg *.bbl *.brf *.aux *.log $(NAME).pdf *.out *.toc chapter*/main.aux
