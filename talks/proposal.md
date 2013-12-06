# Lattice-based data structures for deterministic parallel and distributed programming

## Title slide

Thanks for being here at 8:30!  I really appreciate that everyone is here, including not only my committee but also some people who appear to be here of their own free will.  If anyone wants to take a moment to grab more coffee and bagels before I jump into the talk, now would be a good time to do so.

...

OK, great!  I'm going to get started, and please feel free to ask questions at any point.  This proposal is about lattice-based data structures for deterministic parallel and distributed programming.  I'm not ordinarily a big fan of outline slides, but this one time I decided I'd better make one so that you know what to expect.

## Outline for this talk

I'm going to start out by talking about the problem that this work is broadly concerned with, which is the problem of how to do guaranteed-deterministic parallel programming, and I'll explain what that means.  I'll also talk about the existing approaches that our approach generalizes.

Then, I'll give an overview of our approach, and in this part I'll explain how lattice-based data structures, or LVars, work.

After that I'll explain how we've extended the LVars model, in a way that results in what we call a quasi-deterministic parallel programming model, and I'll explain what quasi-determinism means.

Then, I'll talk about how the idea of LVars has been put into practice in our Haskell library for guaranteed-deterministic and quasi-deterministic parallel programming with LVars, which is called LVish.

After that, I'll explain the relationship between LVars and a line of work from the distributed systems community on conflict-free replicated data types, or CRDTs, and I'll talk about how I plan to leverage that relationship.

Finally, I'll end with my plan for completing the pieces of this research that aren't done.   The majority of what I'm talking about describes work that's already published. In particular, the basic LVars model already exists, and we've defined a language based on it and proved it deterministic; the extension that introduces quasideterminism is done, and we've proved it quasi-deterministic; and a version of the LVish library already exists.

The part that I haven't done yet is the work that has to do with CRDTs, but I've included the LVish library here under both `already done` and `still to do` because, as you'll see, I'm planning to extend it to relate to this last piece on CRDTs.

## Outline: The problem and existing approaches

So, let's start with some background on the problem of how to do guaranteed-deterministic parallel programming, and existing approaches to the problem.

## Deterministic parallel programming

First of all, what do I mean by deterministic parallel programming?  By parallel programming, in this proposal, I mean writing programs in such a way that they can be run on parallel hardware, with the goal of making them go faster.  (By parallel hardware, I mean multiple cores, or independent execution units, or computing stations, or whatever you want to call them.)

One of the fundamental reasons why this is hard is that when you have programs running in parallel, they can behave unpredictably, reflecting the unpredictable way in which parallel tasks interact.  This is exacerbated by the fact that a lot of parallel algorithms are so-called "irregular" parallel algorithms, where the work the algorithm needs to do depends on the shape of the data being operated on, so the work has to be dynamically scheduled, and because you can't know the schedule in advance, you can't predict inter-task interactions that come about as a result of scheduling.

To cope with this, we have deterministic parallel programming models.  By deterministic parallel programming, I mean parallel programming, but in such a way that the observable result of a program is guaranteed to be the same on every run, regardless of how it happens to be scheduled onto those multiple cores.  This notion of observable results is important, and I'll come back to this notion of observability later on. I also wat to point out that what I'm going to be concerned with here is not verifying the determinism of individual programs but rather with developing a deterministic-by-construction parallel programming model, such that all programs written in the model are guaranteed to be deterministic.

To give an example of what I mean by unpredictable interactions between parallel tasks, I've written a toy example of a program that exposes schedule nondeterminism.  It happens to be in Haskell, but there's nothing about this example that's particular to Haskell.

## What does this program evaluate to?

So, this program uses something called an MVar, which is a mutable memory location that can be shared among threads and used for asynchronous message-passing between them.

In Haskell, there's a library that provides an MVar data type that supports these `putMVar` and `takeMVar` operations. So we can create a new MVar, and then we can launch a couple of threads, one of which puts 3 into that location and one of which puts 4, and finally we're taking the contents of that location with `takeMVar`, and returning it.

So, what does this program `p` evaluate to?  What are the contents of this MVar, `num`, when `takeMVar` runs?  The answer is that it depends on how the two threads are scheduled.  And just to show you that this is not only a hypothetical concern...

### Don't have to get into this unless someone asks

An MVar is a sort of bounded message queue that can store at most one message at a time, and when someone reads a message, they actually remove it, and leave the queue available to be written to again.  So, for instance, if we write 3 to this MVar and then we try to write 4, then the attempt to write 4 will block until someone takes the 3, reading it out into a variable, like this v, for instance.  Conversely the 4 could get there first and then the attempt to write 3 will block.

## (slide with result of running program)

...this is the result of running this program a few hundred times in parallel on my two-core laptop.  So the answer to the question of what this program evaluates to seems to be usually 4, but occasionally 3.

So, this is an example of what we want to avoid.  And we want to do it by providing a deterministic-by-construction programming model, which would either rule out programs like this from being written, or otherwise force programs that would have behaved like this to instead behave deterministically.

## Disallow multiple writes?

So, how are we going to do that?  Well, determinism-by-construction is a long-standing goal, and there's been a lot of work on it.  So I want to first talk about one of the traditional, standard techniques for enforcing determinism in this setting.

The idea of this technique is to observe that the source of the nondeterminism here is the fact that we were allowed to write to a shared variable more than once.  And so, in order to prevent nondeterminism, the reasoning goes, we should say that shared variables like `num` can only be assigned to once, and never again thereafter.

These single-assignment variables are known as IVars, as opposed to MVars.  ANd in Haskell, there's an IVar data type that we can use inside what's called a Par computation. So, we can easily translate this program to use IVars, and therefore turn it into a deterministic-by-construction parallel program.  Instead of having `p` be an ordinary `IO` computation, which is what it was before, we instead have it be a `Par` computation, and instead of an `MVar` for `num`, we can instead use an `IVar`.  When we do this, then this program raises a run-time error, saying that we tried to write twice to the same `IVar`, and it raises that error deterministically.

We should also note that in addition to being single-assignment, IVars also have blocking read semantics, meaning that if you try to look at the contents of an IVar while it's still empty, then your read will block until some other thread comes along and fills that IVar with a value.

## Disallow multiple writes?

Now, IVars are an old idea.  The earliest reference that I've been able to find for single-assignment variables is a 1968 paper by Tesler and Enea which introduced a single-assignment language called "Compel", which was short for "compute parallel".  (And incidentally, this is the same Larry Tesler who's perhaps better known for numerous HCI contributions, including for his work at Xerox PARC a few years later as one of the inventors of cut/copy/paste.  So single-assignment seems to have been around as a technique for longer than the cut/copy/paste metaphor has been around.)  And today, IVars are a popular approach to doing deterministic-by-construction parallel programming.  They're in Concurrent ML; they're in the Intel Concurrent Collections system for deterministic parallel programming, they're in the Akka library for doing dataflow programming in Scala, and they're in Haskell, as we saw.  So, IVars are popular, they've stood the test of time, and they work; what's not to like?

Well, the single-write restriction on `IVar`s (when coupled with blocking reads) prevents us from writing nondeterministic programs, which is great. Unfortunately, `IVar`s also prevent us from writing certain _deterministic_ programs that we might like to be able to write.

## Deterministic programs that single-assignment forbids

As a simple example, let's tweak our code a tiny bit, so that, instead of trying to write _different_ values into an `IVar` from two different threads, it tries to write the _same_ value from two different threads.

Instead of threads racing to write `3` and `4`, we now have threads that are racing to write `4` and `4`.  It's clear that `v` should deterministically end up as `4`, regardless of who wins the race.  So, this is a deterministic program, right?  What do you think is going to happen when I run it?

Because of the single-write restriction on IVars, this program raises a run-time error, just as the previous one did.

## What's so bad about this?

So, we have a deterministic program that's being ruled out by the single-write restriction.  But who cares?  Why would we ever want to write the same thing twice?  So let me give a reason why we might want to.

Let's suppose we want to implement a _bitwise trie_ data structure into which we can insert strings of bits. Whenever we do an insertion, it forms a chain of nodes from root to leaf, where each node represents a bit.  So this is a picture of a bitwise trie into which we've already inserted some strings of bits -- say, `'0'`, `'1100'`, and `'1111'`.

Suppose we wanted to implement a concurrent bitwise trie -- one that allows safe concurrent insertions.  Could we use `IVar`s to represent the nodes of the trie?  At first, it seems like it would work OK.  Concurrently inserting the strings `'0'` and `'1100'` would work fine, since they don't overlap at all.  Indeed, _any_ data structure built out of `IVar`s works fine in general, as long as writes never overlap.

But suppose that we want to concurrently insert two strings that happen to share a prefix -- suppose, for instance, that one thread is trying to insert `'1100'` while another is trying to insert `'1111'`.  Now we have a dilemma, because these two writes are overlapping.  If one thread writes into the topmost `IVar` and the other thread has gotten there first, we'll raise a multiple-`put` error.  So we need to check for emptiness before doing a write -- but we can't, because there's no way to check an `IVar` for emptiness.  So, each thread will have to block waiting for the other -- _even though the result of the two writes would be the same regardless of the order in which they occurred_.  This would be really easy to accomplish if `IVars` allowed multiple writes of the same value, but as it stands, it wouldn't work.

This is where LVars come in: they retain the determinism of `IVar`s, but they're flexible enough to allow us to write deterministic parallel programs like the ones on this slide.

## Outline: Our approach: LVars

So, now I'll explain what LVars are and how they work.

## LVars: multiple monotonic writes

So what are LVars?  Like MVars and IVars, _LVars_ are locations that can be shared among multiple threads.  Like IVars, they ensure determinism: a program in which all communication takes place over LVars (and in which there are no other side effects) is guaranteed to evaluate to the same value every time.

Unlike IVars, though, LVars allow multiple _monotonic_ writes.  That is, it's fine to write to an LVar multiple times, with the caveat that the LVar's contents must _stay the same or grow bigger_ with each write.  This is guaranteed to be the case because the `put` operation updates the contents of the LVar to the _least upper bound_ of the previous value and the new value.

Let's look at the simplest possible example.  Here we have a lattice representing the states that a natural-number-valued IVar can take on.  The bottom element of the lattice represents the "empty" state of the IVar.  The elements `0`, `1`, `2`, and so on represent each of the possible "full" states.  Finally, the top element of the lattice represents the error state that results from incompatible writes.  We can see that the least upper bound of any two _different_ "full" states (say, `3` and `4`, for instance) is top, and this corresponds to the fact that if we tried to write both of those values, we'd get an error, as we'd expect.  But the least upper bound of any element and _itself_ is just that element,  and this matches our intuition that repeated writes of the same value to an IVar ought to be fine with regard to determinism.

Both these examples, by the way, are written using our Haskell implementation, which I'll talk about later on.

So, my claim is that the lattice-based semantics of LVars is a natural fit for IVars; in fact, _IVars turn out to be a special case of LVars_.  The only API difference between traditional IVars and IVars implemented using this lattice-based semantics is that these support repeated writes of the same value without exploding at run-time.

## Overlapping writes are no problem

For instance, if we go back to the this bitwise trie example I mentioned earlier, now overlapping writes are no problem.  If we use lattice-based IVars instead of traditional IVars, then the two inserting threads could happily race to write their shared prefix, with no risk of nondeterminism, since they're both writing the same thing.

But, moreover, now that we can do multiple least-upper-bound writes, IVars are just the beginning; and now we can have LVars whose states correspond to all kinds of lattices.  So next, I'm going to show a more interesting example that will also illustrate how LVar reads work.

## LVars: threshold reads

In this example, we're looking at a LVar whose states are pairs of single-assignment variables -- so, essentially it's a pair of IVars.  Let's walk through this program, which is again written using our library, and see how the state of the LVar changes as we go along.

We start out by creating a new pair IVar by calling `newPair`, and its state starts out as bottom, the least element of the lattice.  Next, we fork a couple of threads, each of which does something: one calls `putFst`, which writes its argument into the first component of the pair, and the other calls `putSnd`, which writes into the second component.  Those two operations could occur in either order.  Finally, there's `getSnd`, which we'll talk about in a moment.

[Walk-through of lattice states]

Now, we mentioned earlier that IVar reads are blocking reads -- if you try to read from an IVar before it's been filled in, you won't be able to.  However, once the read unblocks, you're allowed to read the exact, complete contents of the IVar.

With LVars, however, it's a little different, and this is where we return to that notion of observability that I mentioned earlier. I see it as a determinism-preserving tradeoff: LVars are a generalization of IVars that let you do multiple monotonic assignment in exchange for having to make more limited observations of the contents of the lattice.  So how do these limited observations work?

First of all, they're blocking, just as IVar reads are -- so if we tried to run `getSnd` before the second element of the pair had been filled in, then our attempted read would block until that element appeared.  However, as we can see from the lattice, there are two possible states that the LVar might be in at the time that it unblocks: it could be in the state (0, 1), if it also happens to be the case that the first element has been written, or it could be in the state (bottom, 1), if the first element hasn't been written yet.

If our read were allowed to distinguish between these two states, then that would break determinism.  So, instead, LVar reads are what we call "threshold reads".  We designate a subset of elements in the lattice that serve as a threshold, such that if the actual state of the LVar reaches a point at or above any of those elements, then the read can unblock and return a result.  However, the result that it returns is *not* going to be the exact LVar state, but just the element of the threshold set that the exact state is currently at or above.  So, for example, regardless of whether we're in state (0, 1) or (bottom, 1), the call to getSnd will return (bottom, 1).  (That's what it does in the formalism; in the implementation it just returns 1.)

In order for this behavior to be deterministic, there has to be a _unique_ element of the threshold set that every element of the lattice is at or above.  We ensure this by requiring that the threshold set be _pairwise incompatible, meaning that for every two elements in the threshold set, their least upper bound is Top.

One thing that helps in understanding threshold sets is to visualize the threshold set as a tripwire going across the lattice.  The LVar's state moves up in the lattice, and eventually it may cross the tripwire.  At that point, we're allowed to unblock and return a result.  But that result will be the same on every run, regardless of where we happened to cross the tripwire on any given run.

Also, I want to point out that these threshold sets don't aactually appear in end-user programs written using our library; it's up to the data-structure implementor to think about only exposing operations like `getFst` and `getSnd` that *could* be expressed with threshold sets.  So you don't even have to think about threshold sets unless you actually want to implement your own LVar types in addition to all the ones we already provide.

## Outline: Quasi-determinism with LVars

Thus far I've talked about how LVars enable guaranteed-deterministic parallel programming.  However, the programming model of monotonic writes and threshold reads is quite limited.  So next I want to briefly discuss an extension to the model that makes it more expressive, but that pushes it into the realm of what we call quasi-determinism, and I'll explain what that means.

## Problem: Threshold reads can't say "no"

We talked about how threshold reads are blocking.  This means that in the LVars model, the answer to the question of "has a given write occurred?" is always "yes"; we might just have to wait a while before we get that "yes" answer, because the reading thread will block until the LVar's contents reach the threshold.

Unfortunately, this makes it hard to express certain algorithms. Consider, for instance, a graph traversal algorithm that finds the connected component of a graph starting from a given node.  As we traverse the graph, the set of nodes that we've seen grows monotonically, and we continue adding the neighbors of seen nodes to that set until the set reaches a fixed point.  Because this algorithm involves a monotonically growing data structure, it would seem at first glance to be a perfect match for the LVars model.  Unfortunately, we can't express it using only the threshold reads that I described.

The problem is that the algorithm relies on being able to find out negative information about that monotonically growing data structure.  That is, we should only explore neighboring nodes should if the current node is not in the set of seen nodes; a fixpoint is reached only if no new neighbors are found; and, of course, at the end of the computation we want to learn exactly which nodes are in the connected component, which entails learning that certain nodes were not in it.  I want to stress that the set of nodes in the connected component will be deterministic for any given input -- the input being a graph and a particular start node.  So, because the result of the computation is deterministic, it should in principle be possible to express it with a program written in a guaranteed-deterministic programming model!

So it turns out that it is possible to extend the LVars model to make it possible to express computations like this, and we extend it in two ways.

## Solution: LVar operations beyond `put` and `get`

First, in addition to `put` and `get`, which are the usual least-upper-bound write and threshold read operations that I described earlier, we add a new primitive operation called `freeze`.  The `freeze` operation is a non-blocking read that lets us find out the exact contents of the LVar, unlike `get` which only gives us an underapproximation of the LVar's contents.

Freezing imposes a trade-off.  Once an LVar has been frozen, any further writes that would change its value instead raise an exception.  On the other hand, it becomes possible to discover the exact value of the LVar, learning both positive and negative information about it, without blocking.  This means that LVar programs that use `freeze` are *not* guaranteed to be deterministic, because they could nondeterministically raise an exception depending on how `put` and `freeze` operations are scheduled.  However, we *can* guarantee that such programs satisfy a weaker property called *quasi-determinism*, which is the property that all executions that produce a final value produce the *same* final value, but some executions might raise an exception.

Second, we add the ability to attach event handlers to an LVar.  When an event handler has been attached to an LVar, it invokes a callback function to run, asynchronously, whenever that LVar is updated monotonically.  We can't check for emptiness of an LVar, but we *can* check for quiescence of a group of handlers, meaning that no callbacks are enabled.  Since quiescence means that there are no further changes to respond to, it can be used to tell that a fixpoint has been reached.  Of course, since more events could arrive later, there is no way to guarantee that quiescence is permanent -- but, since the contents of the LVar being written to can only be read through `get` or `freeze` operations anyway, early quiescence poses no risk to determinism or quasi-determinism, respectively.

In fact, freezing and quiescence work particularly well together because freezing provides a mechanism by which we can safely "place a bet" that all writes have completed.  So we can use freezing and handlers to implement the graph traversal algorithm that I described earlier.

This code is written using our LVish library.  The `Par` type constructor is the monad in which LVar computations live.  The `traverse` function discovers (in parallel) the set of nodes in a graph `g` reachable from a given node `startNode`, and is guaranteed to produce a deterministic result.  It works by creating a new `Set` LVar (corresponding to a lattice whose elements are sets, with set union as least upper bound), and seeding it with the starting node.  The `freezeSetAfter` function combines freezing and event handling..  First, it installs the callback function `f` as a handler for the `seen` set, which will asynchronously put the neighbors of each visited node into the set, possibly triggering further callbacks, recursively.  Second, when no further callbacks are ready to run -- or, in other words, when the `seen` set has reached a fixpoint -- `freezeSetAfter` will freeze the set and return its exact value.

Now, the good news is that this particular program is deterministic.  The bad news is that, in general, freezing introduces quasi-determinism, because we might freeze an LVar and then do a `put` to it later, which would raise an error.  However, if we can ensure that the freeze does indeed happen after all writes have completed, then the computation is guaranteed to be deterministic, and it is actually possible to enforce this "freeze-last" idiom at the implementation level.  I'll talk about that next.

## Outline: The LVish library

I've already been showing you code examples throughout the talk.  These code examples are written using our Haskell library for guaranteed-deterministic and quasi-deterministic parallel programming with LVars, which is called LVish, and next I want to give a short overview of that.

## The LVish library

The LVish library provides a monad, called `Par`, in which LVar computations run.  This is a generalization of the original `Par` monad, which was provided by the "monad-par" library.  So monad-par is also a library for deterministic parallelism in Haskell, and it allows determinism-preserving communication between threads, but only through IVars, rather than LVars.  So LVish is a generalization of that.

The LVish library is really two things.  First, it's the implementation of this `Par` monad, which of course has to include a way to run a `Par` computation, and when you run a `Par` computation, it is dynamically scheduled by a work-stealing scheduler.  So the library includes that scheduler implementation, which is interesting in its own right.   Our POPL paper describes the scheduler implementation, so I'm not going to discuss any of that in detail -- all I really want to say is that `Par` computations launch library-level threads that are scheduled by this library-level scheduler.

None of what I've discussed about LVars in this talk so far is specific to Haskell.  But because we did decide to implement it in Haskell, we're able to do some cool tricks that I want to talk about briefly.  One thing about `Par` that I want to be sure to mention is that `Par` computations are parameterized by an *effect level*, which allows for fine-grained specification of the effects that a given computation is allowed to perform.  For instance, since `freeze` introduces quasi-determinism, a computation indexed with a deterministic effect level is not allowed to use `freeze`.  So, the *static type* of an LVish computation reflects its determinism or quasi-determinism guarantee.

But we can do even finer-grained effect tracking -- for instance, you can use it to say that a computation can perform `get`s but not `put`s or `freeze`s -- in other words, not only is it deterministic, it can't even side-effect an LVar -- then we should be able to end that computation early and save cycles, so in other words, the computation is "cancellable".  Cancellation is an effect that we can implement with a monad transformer on top of a `Par` computation that's marked as cancellable.

Moreover, if a `freeze` is guaranteed to be the last effect that occurs in a computation, then it is impossible for that `freeze` to race with a `put`, ruling out the possibility of a run-time `put`-after-`freeze` exception.  Hence a computation in which any occurrence of `freeze` is guaranteed to occur last is deterministic, and LVish exposes a `runParThenFreeze` operation that captures this "freeze-last" idiom and has a deterministic effect level.  The graph traversal that I showed before did the `freeze` manually at the end of a normal `Par` computation, but it could have instead *not* done that, and instead been run with `runParThenFreeze`, in which case it would be guaranteed deterministic!

So far, I've described the `Par` monad that LVar effects run in.  What about the LVar data structures themselves that are *being* side-effected by all this?  The library also provides a bunch of basic LVar data structures: IVars, IStructures, sets, maps, and so on.  In addition to the provided ones, users can implement their own LVar types, and the library provides some handy stuff to facilitate the definition of user-defined LVars.  There are certain proof obligations that the LVar implementor has to meet -- namely, they have to ensure that their `put` operation actually computes a least upper bound and that their `get` operations can be understood as corresponding to incompatible threshold sets.

To illustrate how LVish is used, I'll draw on my collaborators' and my experiences using the LVish library and describe three case studies, all of which make use of handlers and freezing.  First, I'll show the graph traversal that I showed earlier.  Second, I'll describe using LVish to parallelize a _k_-CFA control flow analysis algorithm.  Finally, I'll describe using LVish to implement PhyBin, which is a bioinformatics application for comparing genealogical histories.

This last one relies on a parallel tree-edit distance algorithm that finds the edit distance between all pairs of trees in a set of trees, filling in all the cells in a distance matrix.  It's particularly interesting because it relies on the ability to perform writes to LVars that are commutative and inflationary with respect to the lattice in question, but *not* idempotent (in contrast to the least-upper-bound writes discussed above, which are idempotent).  For instance, an operation that increments a counter LVar is commutative and inflationary but not idempotent.  These non-idempotent writes, which we call `bump` operations, preserve determinism as long as programs do not use `put` and `bump` on the same LVar, a property which can be statically enforced by the aforementioned effect specification system in LVish.  I don't actually have a proof yet that `bump` is deterministic, but I *will*, and I'll come back to that in a minute.

## Outline: Joining forces: LVars and CRDTs

Finally -- and this is the last technical material in the talk -- I want to discuss the relationship between LVars and what are known as "conflict-free replicated data types", or CRDTs, which are a way of specifying the behavior of replicated objects in a distributed system, and I'll talk about how I plan to leverage that relationship.

## (TODO: slides about CRDTs)

(TODO)

## Outline: Research plan

To wrap the talk up, I'm going to say something about what work is already done and what remains to be done, and my plan for doing it.

## What's already done

Together with my collaborators, I've already done a lot of the work described in this proposal.

First, we came up with the basic LVars model (with `put` and `get` operations), and I formally defined a parallel calculus with LVars, lambdaLVar; I implemented it in PLT Redex; and I proved determinism for it.  This appeared in our FHPC '13 paper.

Then, I formally defined what we called the LVish calculus (which I'm now calling lambdaLVish in this proposal so as not to confuse it with the LVish Haskell library), which extends the original model with freezing and event handlers; I implemented *that* in PLT Redex; and we proved quasi-determinism for it, and that result is in our POPL paper.

Finally, we implemented and released the LVish Haskell library based on the formal LVars model, and our POPL paper covers the implementation of LVish and gives a couple of case studies of programming with it.

That paper doesn't cover everything I mentioned with regard to the LVish implementation; the rest is in a new paper on LVish that's currently under submission to PLDI, and this new paper covers the fine-grained effect tracking system and the non-idempotent `bump` operations I mentioned, as well as the PhyBin case study that relies on them.

## Still to do

What I plan to do next:

First, I want to define the semantics of `bump` and add it to lambdaLVish.  I want to actually prove determinism for the subset of lambdaLVish, that includes `bump`, but does not include freezing and event handlers (this proof should be a straightforward refactoring of the existing determinism proof for lambdaLVar).  And, I want to update the existing quasi-determinism proof for lambdaLVish to account for `bump`.  I estimate about a month for all this.

Then, I want to jump into the last stage of the work, which is to exploit the CRDT/LVars relationship in both directions.  First, I want to extend the definition of CvRDTs to include threshold reads, and I want to define and prove a query consistency property for CvRDTs extended thusly.  I expect to spend about two months on this.
  
Second, I want to implement some CRDT-inspired LVars in the LVish library -- PN-Counters and 2P-Sets, to be specific -- and release a new version of LVish with them.  I want to implement at least one application that makes use of them, as well.  I expect to spend about three months on this.

Finally, I expect to spend about three months writing.  We want to do an extended journal version of the POPL paper, which I expect will include this material on `bump`.  I also want to write a new paper on CRDTs and LVars.  And finally I need to integrate the material from all these papers into the dissertation itself.

Altogether, this plan puts me on track to defend, in September 2014, and my thesis is that:

## Thesis statement

Lattice-based data structures are a general and practical foundation for deterministic and quasi-deterministic parallel and distributed programming.

That's all I have!  Thanks again.
