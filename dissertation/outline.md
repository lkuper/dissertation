## Outline

1. Introduction
  1. The deterministic-by-construction parallel programming landscape
  2. Lattice-based, monotonic data structures as a basis for deterministic parallelism
  3. Quasi-deterministic and event-driven programming with LVars
  4. The LVish library
  5. Deterministic threshold queries of distributed data structures
  6. Thesis statement, and organization of the rest of this dissertation
  7. Previously published work

2. LVars: lattice-based data structures for deterministic parallelism
  1. Motivating example: a parallel, pipelined graph computation
  2. LVars by example
  3. Lattices, stores, and determinism
  4. lambdaLVar: syntax and semantics
  5. Proof of determinism for lambdaLVar
  6. Generalizing the `put` and `get` operations

3. Quasi-deterministic and event-driven programming with LVars
  1. lambdaLVish, informally
  2. lambdaLVish, formally
  3. Proof of quasi-determinism for lambdaLVish

4. The LVish library: interface, implementation, and evaluation
  1. The big picture
  2. The LVish library interface for application writers
  3. Par-monad transformers and disjoint parallel update
  4. The LVish library implementation
  5. Case study: parallelizing k-CFA with LVish
  6. Case study: parallelizing PhyBin with LVish

5. Deterministic threshold reads of distributed data structures
  1. Background: CvRDTs and eventual consistency
  2. Adding threshold queries to CvRDTs
  3. Determinism of threshold queries

6. Related work
  1. Deterministic Parallel Java
  2. FlowPools
  3. Concurrent Revisions
  4. Conflict-free replicated data types
  5. Bloom and Bloom^L
  6. (Related work in separation logic, maybe?)

7. Summary and future work
  1. Remapping the deterministic parallel landscape
  2. Distributed programming and the future

### Appendices

A. Proofs

B. PLT Redex models of lambdaLVar and lambdaLVish
