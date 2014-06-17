## Outline

1. Introduction
  * will be much like sections 1/2 of my thesis proposal
  * introductory material from FHPC/POPL/DISC papers
  * thesis statement

2. LVars: Lattice-based Data Structures for Deterministic Parallelism
  * will be like on sections 3.1-3.5 of my thesis proposal (but slower-paced and more example-driven)
  * LVar basics from the FHPC paper, but updated to use the POPL paper's formalism minus handlers/quiescence/freezing
  * explain lattices
  * explain basic threshold reads
  * explain generalized threshold reads (use example from DISC paper)
  * determinism proof outline

3. Quasi-Deterministic and Event-Driven Programming with LVars
  * will be like sections 3.6-3.7 of my thesis proposal (but slower-paced and more example-driven)
  * introduce handlers/quiescence/freezing, by example, as in the POPL paper
  * add handlers/quiescence/freezing to the semantics
  * quasi-determinism proof outline

4. The LVish Library: Implementation, Benchmarks, and Case Studies
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
