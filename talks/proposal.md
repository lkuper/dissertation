# Title slide

Thanks for being here at 8:30; I really appreciate that everyone is here, including not only my committee but also some people who appear to be here of their own free will, so thanks for being here, and if anyone wants to take a moment to grab more coffee and bagels before I jump into the talk, now would be a good time to do so.

...

OK, great!  So my proposal is about lattice-based data structures for deterministic parallel and distributed programming.  I'm not ordinarily a big fan of outline slides, but this one time I decided I'd better make one so that you know what to expect.

# Outline for this talk

I'm going to start out by talking about the problem that this work addresses, which is the problem of how to do guaranteed-deterministic parallel programming, and I'll explain what that means.  I'll also talk about the existing approaches that our approach generalizes.

Then, I'll give an overview of our approach, and in this part I'll explain how lattice-based data structures, or LVars, work.

After that I'll explain how we've extended the LVars model to what we call a quasi-deterministic parallel programming model, and I'll explain what that means.

Then, I'll talk about how the idea of LVars has been put into practice in our Haskell library for guaranteed-deterministic and quasi-deterministic parallel programming with LVars, which is called LVish.

After that, I'll explain the relationship between LVars and a line of work from the distributed systems community on conflict-free replicated data types, or CRDTs, and I'll talk about how I plan to leverage that relationship.

Finally, I'll talk about the related work that I won't have already covered, and then I'll talk about my plan for completing the pieces that aren't done.

A big chunk of this talk describes work that's already published. In particular, the basic LVars model already exists, has been formally defined and proved deterministic, and the extension to quasideterminism is done, and has been proved quasi-deterministic, and a version of the LVish library already exists.  The part that I haven't done yet is the work that has to do with CRDTs, and I've included the LVish library here under both `already done` and `still to do` because, as you'll see, I'm planning to extend it to relate to this last piece on CRDTs.

## The problem and existing approaches

Parallel programming means programming in such a way that programs can be scheduled onto multiple cores (or independent execution units, or computing stations, or whatever you want to call them) to make them run faster.

Guaranteed-deterministic parallel programming means doing that, but in such a way that the observable result of a program is guaranteed to be the same on every run, regardless of how it happens to be scheduled onto those multiple cores.

  * Parallel programming is hard.  One of the things that makes it
    hard is that programs can behave unpredictably when run in
    parallel, due to unpredictable interactions between parallel
    tasks.
	
  * "Irregular" parallel programs have to be dynamically scheduled,
    and because you can't know the schedule in advance, you can't
    predict inter-task interactions that come about as a result of
    scheduling.
	
  * Deterministic-by-construction parallel programming models counter
    this by restricting the ways in which parallel tasks can interact.


# Outline of proposal document

  * Introduction
    * Existing approaches to determinism by construction
	* LVars: lattice-based monotonic data structures
	* Quasi-deterministic programming with LVars
	* LVars and conflict-free replicated data types
	* The LVish library
  * Thesis statement
  * Technical overview
    * Lattices
	* Freezing
	* Stores
	* The lambdaLVish language
	* Semantics of `new`, `put`, and `get`
	* Semantics of freezing and event handling
	* Quasi-determinism proof outline
  * Joining forces: LVars and conflict-free replicated data types
    * Replication and eventual consistency
	* Resolving conflicts between replicas
	* Preliminaries
	* State-based objects and CvRDTs
	* CmRDTs and non-monotonic updates
	* Threshold reads of non-monotonic data structures
  * Related work
    * Deterministic Parallel Java (DPJ)
	* FlowPools
	* Concurrent Revisions
	* Bloom and Bloom^L
  * Road map
