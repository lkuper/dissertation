## Outline

1. Introduction
  1.1. The deterministic-by-construction parallel programming landscape
  1.2. Lattice-based, monotonic data structures as a basis for deterministic parallelism
  1.3. Quasi-deterministic and event-driven programming with LVars
  1.4. The LVish library
  1.5. Deterministic threshold queries of distributed data structures
  1.6. Thesis statement, and organization of the rest of this dissertation
  1.7. Previously published work

2. LVars: lattice-based data structures for deterministic parallelism
  2.1. Motivating example: a parallel, pipelined graph computation
  2.2. LVars by example
  2.3. Lattices, stores, and determinism
  2.4. lambdaLVar: syntax and semantics
  2.5. Proof of determinism for lambdaLVar
  2.6. Generalizing the `put` and `get` operations

3. Quasi-deterministic and event-driven programming with LVars
  3.1. lambdaLVish, informally
  3.2. lambdaLVish, formally
  3.3. Proof of quasi-determinism for lambdaLVish

4. The LVish library: interface, implementation, and evaluation
  4.1. The big picture
  4.2. The LVish library interface
  4.3. Par-monad transformers and disjoint parallel update
  4.4. The LVish library implementation
  4.5. Case study: parallelizing k-CFA with LVish
  4.6. Case study: parallelizing PhyBin with LVish

5. Deterministic threshold reads of distributed data structures
  5.1. Background: CvRDTs and eventual consistency
  5.2. Adding threshold queries to CvRDTs
  5.3. Determinism of threshold queries

6. Related work
  6.1. Deterministic Parallel Java
  6.2. FlowPools
  6.3. Concurrent Revisions
  6.4. Conflict-free replicated data types
  6.5. Bloom and Bloom^L
  (6.6. Related work in separation logic <-- maybe?)

7. Conclusion
  7.1. Remapping the deterministic parallel landscape
  7.2. Distributed programming and the future

### Appendices

A. Proofs

B. PLT Redex Models of lambdaLVar and lambdaLVish
