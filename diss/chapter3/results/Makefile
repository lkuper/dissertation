

all: deps plotem

plotem:
	runghc-7.6.3 ./plot_ALL.hs ./cultivated/

# Convenience: install dependencies for the scripts.
deps:
	cabal install prettyclass HSH

clean:
	rm -rf *.exe *.o *.hi cultivated/*_graphs
