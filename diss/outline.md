## Outline

1. Introduction
  1.1. The deterministic-by-construction parallel programming landscape
  1.2. Lattice-based, monotonic data structures as a basis for deterministic parallelism
  1.3. Quasi-deterministic and event-driven programming with LVars
  1.4. The LVish library
  1.5. Deterministic threshold queries of distributed data structures
  1.6. Thesis statement, and organization of the rest of this dissertation
  1.7. Previously published work

2. LVars: Lattice-based Data Structures for Deterministic Parallelism
  2.1. Motivating example: a parallel, pipelined graph computation
  2.2. Lattices, stores, and determinism
  2.3. lambdaLVar: syntax and semantics
  2.4. Proof of determinism for lambdaLVar
  2.5. Generalizing the `put` and `get` operations

3. Quasi-Deterministic and Event-Driven Programming with LVars
  3.1. LVish, informally
  3.2. LVish, formally
  3.3. Proof of quasi-determinism for lambdaLVish

4. The LVish Library: Implementation and Evaluation
  * the LVish implementation overview from the POPL paper
  * idea: combine evaluation sections from all our papers that have them
  * Benchmarks from the FHPC paper?  (Maybe not -- these are basically superseded by the POPL paper.)
  * MIS and BFS benchmarks from the POPL TR
  * k-CFA case study from the POPL paper
  * PhyBin case study from the PLDI paper

5. Deterministic Threshold Reads of Distributed Data Structures
  * loosely based on section 4 of my thesis proposal
  * basically exactly the DISC paper, minus the related work and the LVars background that's already covered earlier

6. Related Work
  * loosely based on section 5 of my thesis proposal
  * idea: combine related work sections from FHPC, POPL, DISC papers
  * related work from PLDI paper?  (Probably not -- it's not that related to the thesis)
  * Point to Praveen's graph algorithms stuff? (maybe this should go in the Evaluation chapter?)

7. Conclusion
  * incorporate concluding points from FHPC, POPL, PLDI and DISC papers
