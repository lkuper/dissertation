# Lattice-based Data Structures for Deterministic Parallel and Distributed Programming

## Abstract

Deterministic-by-construction parallel programming models guarantee that programs have the same observable behavior on every run, promising freedom from bugs caused by schedule nondeterminism.  To make that guarantee, though, they must sharply restrict sharing of state between parallel tasks, usually either by disallowing sharing entirely or by restricting it to one type of data structure, such as single-assignment locations.

I show that *lattice-based* data structures, or *LVars*, are the foundation for a guaranteed-deterministic parallel programming model that allows a more general form of sharing.  LVars allow multiple assignments that are inflationary with respect to an application-specific lattice.  They ensure determinism by allowing only inflationary writes and "threshold" reads that block until a lower bound is reached.  After presenting the basic LVars model, I extend it to support *event handlers*, which enable an event-driven programming style, and non-blocking "freezing" reads, resulting in a *quasi-deterministic* model in which programs behave deterministically modulo exceptions.

I demonstrate the viability of the LVars model with *LVish*, a Haskell library that provides a collection of lattice-based data structures, a work-stealing scheduler, and a monad in which LVar computations run.  LVish leverages Haskell's type system to index such computations with *effect levels* to ensure that only certain LVar effects can occur, hence statically enforcing determinism or quasi-determinism. I present two case studies of parallelizing existing programs using LVish: a *k*-CFA control flow analysis, and a bioinformatics application for comparing phylogenetic trees.

Finally, I show how LVar-style threshold reads apply to the setting of *convergent replicated data types* (CvRDTs), which specify the behavior of eventually consistent replicated objects in a distributed system.  I extend the CvRDT model to support deterministic, strongly consistent *threshold queries*.  The technique generalizes to any lattice, and hence any CvRDT, and allows deterministic observations to be made of replicated objects before the replicas' states converge.

## (title; intro)

Good afternoon!  Thanks for being here.  I'm Lindsey Kuper, and in this talk, I'm going to tell you about my dissertation work on lattice-based data structures for deterministic parallel and distributed programming.

I want to start by motivating why we're all here.  Why are we here?  Why are you here?  Well, if you're my mom and dad, you're here because I'm your daughter.  But most of us are here because we study computer science.  So, if you're here, chances are you're aware of, and maybe even interested in, two big problems in CS.

## (parallel systems; distributed systems)

Those problems are, first, how do we effectively program parallel systems?  That is, how do we write programs in such a way that they run on multiple processors, and hence go fast, and yet still compute correctly?

And, second, how do we effectively program distributed systems?  That is, how do we write programs in such a way that they run, correctly, across a network, that's subject to network partitions and other kinds of failures?

And different formalisms and perhaps even different subfields of CS have been developed to deal with these two big problems.  So, it's useful to try to find unifying abstractions that can perhaps help us understand and make progress on both of these problems -- and this is really what motivates me: trying to find unifying abstractions for programming.  So to that end, my dissertation work is about what I call lattice-based data structures, and... 

## (my thesis; a roadmap of sorts)

...my thesis is that lattice-based data structures are a general and practical unifying abstraction for deterministic and quasi-deterministic parallel and distributed programming.

And this thesis serves as a road map for my talk.

First up, I'm going to explain what I mean by "deterministic", what I mean by "parallel programming", and what I mean by "deterministic parallel programming".  And I'm going to talk about some of the existing approaches to the problem of deterministic parallel programming.

Then I'm going to explain what I mean by lattice-based data structures, and as part of that, we'll see how they generalize those existing approaches that I'll cover.  Lattice-based data structures are called LVars for short, and we'll see why they're called that in a little while.

I'm going to introduce the idea of quasi-determinism, which means "determinism modulo errors", as I'll explain later on.  I'll show how we incorporate this idea in the LVars programming model, and why it's such an interesting property.

To support these claims of determinism and quasi-determinism, I actually have to prove that those properties are true.  So I'm going to sketch out for you how I proved that our two flavors of the LVars programming model are deterministic and quasi-deterministic, respectively.

Then, to support my claim of practicality, we're going to look at some real problems that we've solved using the LVars programming model.  I have two case studies to show you.

Most of the talk is going to focus on parallel programming, but finally I'm going to show how the LVars approach applies in the setting of distributed programming, in particular, to distributed replicated data structures.

To begin with, let's talk about parallel programming...

## (deterministic parallel programming)

...and, in particular, about deterministic parallel programming.  What do I mean by that?

Broadly, by parallel programming I mean writing programs in such a way that they can take advantage of parallel hardware to go faster.  By deterministic parallel programming we mean, doing that, but in such a way that the observable results of our program are always the same.  This idea of observability is important, and it'll come up again, but first, so to give some intuition for what deterministic parallelism is about, I'm going to tell a story.  This is one that many of you have heard before.  I like to tell it at parties.  So if you've heard it, bear with me!

## (a shopping trip)

Let's suppose that, one day, I'm shopping at my favorite large corporate online store, and right now my shopping cart is empty.  Now it just so happens that my favorite online store has written all of their shopping cart infrastructure in Haskell.  But the ideas we're talking about here aren't unique to Haskell in any way, so I'll explain what this code is doing and feel free to mentally substitute any language of your choice.

So they've defined a data type listing all the items available for purchase, and they're representing the contents of my shopping cart as a key-value `Map`, where the keys are `Item`s, and the values are the quantities that I'm buying of those `Item`s.  Furthermore, this `Map` is stored in what's known in Haskell as an `IORef`, which is a location that can be shared between parallel tasks.  And when that `IORef` is first created, by calling `newIORef`, its  initial contents are an empty map, reflecting the fact that my cart is empty.

So, all of the `Item`s that I buy have to get added to this `Map`.  In particular, this is a store known for its fine selection of books, so I add a book to my shopping cart, and doing so launches a thread that modifies the contents of my cart and adds the book I picked out by calling the `insert` function on the map, with a quantity of 1.  And this happens asynchronously, so that I can keep shopping while that's going on.

But as it turns out, this store also offers all kinds of other fine merchandise, so along with my book, I add a pair of shoes to my cart as well.

Well, then suppose I want to look at the contents of my cart, and suppose that that computation happens in yet another async thread.  This time I care about what the computation returns, so I wait for that thread I've spawned to complete,  I return my cart's contents, and that's what this expression `p` evaluates to.

So, what do you think the contents of my cart are going to be?  What does `p` evaluate to?

Well, I tried running this code myself, and I ran it on parallel hardware, here on my two-core laptop in fact, with the -N2 option enabled so it would use those two cores.  and I ran this program a few hundred times, just to see what would happen.  And the answer seems to be...

## (result of running program)

...it depends.  Well, the first answer I got was both the Book and the Shoes, but sometimes I just got the Shoes, and every now and then I just got the Book.  So, this is a nondeterministic parallel program.  That is, when I look at my shopping cart, what I see is subject to the whims of the language runtime system, and how it chooses to schedule the addition of items to my cart. And that seems less than ideal.

## (we have to tame sharing)

This happened because our program has two tasks that are sharing state with each other.  That is, they're both writing to the same shared data structure.  But it recently occurred to me that "shared" state is kind of a misnomer.  Sharing is a nice thing to do, right?  Sharing is caring!  But these tasks aren't so much *sharing* as fighting viciously over a piece of state.  If we want determinism, we have to learn to share nicely.

## (back to same program again)

Now, if all we wanted to do was fix the nondeterminism in *this* particular program, then we could do that.  We could say, well, the problem is that the program is undersynchronized.  We'd have to put in more synchronization barriers.  In this case, we'd probably have to return something from each of the first two async calls, wait for those actions to finish, and only then do the read.

And, again, this isn't unique to Haskell; this process is similar for other languages, and I would bet that a lot of us have had to do this kind of thing at some point in our lives.  And then we'd be sure to see both of the items we put in our cart.

But, rather than fixing individual programs in an ad-hoc way like this, what we would really like to do is come up with a programming *model* such that all programs written in the model are guaranteed to be deterministic -- in which all programs are guaranteed to share nicely.

## (side-by-side comparison)

In other words, this program -- now that I've fixed it -- *is* deterministic, but not because of any guarantee given us by the programming model; it's deterministic because we put the synchronization barriers in the right places.  By comparison, and to give you an preview of where we're headed, a version of the same program written using the LVars programming model that I'm presenting in this talk is deterministic by construction.

So what does it mean for a program to be deterministic by construction?  Well, it means that the program was written in a language in which deterministic programs are the only programs that can be expressed.  There has been a lot of work done on this over the years and decades.  So before I get into what LVars are, let's take a moment to survey the landscape of deterministic-by-construction parallel programming.

## (the landscape)

One of the classic examples of a guaranteed-deterministic parallel language, of course, is pure functional programming.  Because programs don't have side effects, any two independent expressions can evaluate simultaneously without it changing the eventual value of the program.  With these programs, we don't really have to ask if they share nicely, because they have no shared state at all.

I'm using the lambda calculus as representative of this category, and Haskell belongs here as well, if you're not using IORefs and things like that.  But the key characteristic is lack of side effects, or, in other words, immutability.  So, things like array languages where you manipulate immutable arrays go in this category too.  Starting next week I'm going to be working on River Trail, which adds parallel immutable arrays to JavaScript, and that also falls into this category.

Beginning in the early 1970s, there was work on dataflow models, and in particular Kahn process networks.  These allow message-passing computations in which separate processes can communicate with each other only through blocking FIFO channels, exposing pipeline parallelism.  So with these we get deterministic programming in a dataflow style.

Closely related to those are single-assignment languages, in which parallel computations can share state, but those shared variables can be mutated only once, to initialize them.  Starting in the 80s, these single-assignment structures became known as IStructures, or IVars, and these are the foundation for things like the Intel Concurrent Collections library, or CnC.  And I'm going to talk more about single-assignment in a minute.

And finally, more recently we've seen permissions systems and type systems that make it possible for imperative programs to mutate state in parallel, while guaranteeing that the same state isn't accessed simultaneously by multiple threads.  And DPJ, or Deterministic Parallel Java, is the prototypical example of this approach, which I call imperative disjoint parallelism, and it's yet another way of enforcing nice sharing.

So we've got a bunch of points on the landscape here, and at first appearance, they all describe rather different mechanisms for exposing parallelism and for ensuring determinism.

But, as we uncover these various points in the space, what have we learned about the space as a whole?  Do we know the underlying principles behind these points, or how to unify or generalize them?  It would be nice if we could, because a lot of these mechanisms are fairly narrow.

Now, it might be fine if we just had a big toolkit of unrelated choices, and we just picked the right one for the job as needed, but what do we do when we want to implement an application that has multiple parallelizable kernels that span different points in this space?  For instance, what if we have an application that uses data-flow pipeline parallelism but also does disjoint parallel mutation of a store?

So the question is, can we generalize and unify these points in the space?

## (roadmap)

And the answer is, yes!  All of those points in the space are either subsumed by, or are compatible with, the LVars programming model that I'm going to talk about, because LVars are a general unifying abstraction for deterministic parallel programming.  *And*, LVars can in fact even generalize further, beyond what we saw on the map.

So, in this next part of the talk, I'm going to explain just what LVars are and how they work.

## (cart again)

Let's look back at the nondeterministic version of our shopping cart program.  We can see that it's doing multiple writes to the same shared memory location.  Ruling out programs that do that has been the foundation of a significant body of work on deterministic parallel programming, and a particularly prominent example of that is *IVars*, which are memory locations that can only be written to once, and for which reads block until a write has arrived.

So, in an IVar-based language, you wouldn't be able to write a program with a nondeterminism bug like this; in fact, the combination of single writes and blocking reads ensures determinism for programs in which all communication happens through IVars.  And IVars show up all over the place.  They're in the Concurrent Collections system from Intel for doing deterministic parallelism; they're in the Akka library for dataflow programming in Scala; they've even been implemented in hardware.  And they're very useful!

But, on second thought, is this program really so bad?  Even though these threads both write to the same IORef, notice that neither one really clobbers the other's state; instead, it *builds on* the other's state.  In fact, these first two threads' updates commute, and so it's fine for them to run in arbitrary order.  The nondeterminism is only introduced when the IORef is *read*, and we accidentally observe an intermediate state before both threads have gotten a chance to write.

So, what if we could take inspiration from IVars, loosening the single-write restriction but keeping a form of blocking read that would prevent the order of updates from being observed?  That's what LVars do.

Like IVars, *LVars* are locations that can be shared among multiple threads.  Unlike IVars, though, it's fine to write to an LVar multiple times, with a couple of caveats.  First of all, writes have to be *commutative*; they can happen in arbitrary order.  So, for instance, it doesn't matter whether we write the `Book` or the `Shoes` first.  Second, writes have to be *inflationary*: with every write, the LVar's contents have to *stay the same or grow bigger* with respect to an application-specific lattice of states.  And finally, reads have to be what we call *threshold reads*, which I'll talk about in a moment.

## (the littlest LVar)

A good first example of an LVar is, in fact, an IVar!  So let's look at how that works.  Here we have a lattice representing the states that a natural-number-valued IVar can take on.  The bottom element of the lattice represents the "empty" state of the IVar.  The elements `0`, `1`, `2`, and so on represent each of the possible "full" states.  Finally, the top element of the lattice represents the error state that results from incompatible writes.

Any time you write to this LVar, its contents are updated to what's called the *least upper bound* of the previous state and the new state.  By the least upper bound, we mean the smallest state that's at least as big as both the previous state and the new state.  The least upper bound operation satisfies our criteria for writes -- it's both commutative and inflationary.

We can see that the least upper bound of any two *different* "full" states (say, `3` and `4`, for instance) is top, and this corresponds to the fact that if we tried to write both of those values, we'd get an error, as we'd expect.  But the least upper bound of any element and *itself* is just that element,  and this matches our intuition that repeated writes of the same value to an IVar shouldn't break determinism.

So, the lattice-based semantics of LVars is a natural fit for IVars; in fact, *IVars turn out to be a special case of LVars*.  And in fact the lattice-based version is strictly more expressive -- that is, you can write strictly more deterministic programs with IVars implemented using this lattice-based semantics as opposed to traditional IVars, because the lattice-based ones support repeated writes of the same value without raising a run-time error.

But, now that we can do multiple commutative and inflationary writes, IVars are just the beginning; and now we can have LVars whose states correspond to all kinds of lattices.  So next, let's look at a slightly different lattice and a slightly different program.

[TODO: add an explanation of threshold reads on this slide]

## (a counter LVar)

[TODO: make a slide that uses `incr`; write this section]

Now, in this code, the `incr` and `get` operations I'm using have to respect the properties that I expect of LVar read and write operations.  The `incr` expression has to be commutative and inflationary, and the `get` operation has to do a threshold read.

That was the proof obligation on me when I implemented the Counter data type.  But I didn't have to think about threshold sets or commutative and inflationary writes when I wrote this client code; all I had to do was use what the library gave me.

So in general, the proof obligation on someone who wants to implement a new LVar type is that they have to make sure that their writes are actually commtative and inflationary, and they have to make sure that whatever operations they provide to read from the LVar can be expressed in terms of threshold sets.  If they meet those obligations, then determinism is guaranteed in client code.

So, now we've seen a couple of examples of things you can do with LVars.  But, with what I've shown so far, there are a lot of things you can't do.  Let's consider what they are.

## (what you can't do)

Recall that our original problem was that we wanted to look at our shopping cart and see the contents of it.  With threshold reads, we can sort of do that; we can threshold on the appearance of a particular key and find out what value it points to.  But what if we want to see the whole cart?  And this is a general problem: what if we want to see the exact, complete contents of an LVar?

What if we want to iterate over the contents an LVar?  Maybe it's a container type, like a set or a map, and we want to do something to each element of it.

What if we want to find out whether something is *not* in an LVar?

And finally, what if we want to react to writes that we weren't expecting?  We can't effectively threshold on something unless we know it's going to be written eventually.

So next I'm going to talk about how we extend the LVars model to make it so that we *can* do all these things.  We accomplish this with the addition of three language features: which are called handlers, quiescence, and freezing.

## (freeze after writing)

The key idea is that once all the writes to an LVar are over with, we can *freeze* it, making it so that it can no longer change, and then we can look at its exact, complete contents in a safe way, without having to do a threshold read.  This idea of freezing is what really makes interesting programming possible with LVars.

We called our POPL paper "Freeze After Writing", but another way to put it is "freeze before reading".  In other words, write whenever you want, just make sure that you're done with all the writes before you read.  But how do we enforce freezing, and what happens if we mess up and we accidentally write once we've started reading?

## (roadmap)

Those questions bring me to the next part of the talk, where I'm going to cover quasi-deterministic parallel programming.  I'm going to introduce this through an example problem that's hard to solve using just the threshold reads that we've seen so far.

## (challenge: parallel graph traversal)

All right, so let's say we have a directed graph, like this one, and let's consider the problem of doing a parallel traversal of this graph, starting from a given node and finding all the nodes that are reachable from it.

Ordinarily, the algorithm for doing this might go something like the following: we mark the start node as seen.  Then we look at all its neighbors, perhaps launching a thread for each one, and for each neighbor, we check to see if it's marked as having been seen.  If so, then that thread is done; if not, then we mark it as seen, and then launch a thread for each of its neighbors.  This continues until all the reachable nodes have been seen.

As we traverse the graph, the set of nodes that we've seen grows monotonically, and we continue adding the neighbors of seen nodes to that set until the set reaches a fixed point.  Because this algorithm involves a monotonically growing data structure, it would seem at first glance to be a great match for the LVars model -- we could use an LVar to represent the set of seen nodes, which can only grow.  Unfortunately, we can't express this algorithm using only the threshold reads that I described, because there's no way to check if a node has been seen or not.  And, without that test, we won't know when to stop looking for new nodes.

## (events and event handlers)

In this graph, for instance, after we add node 11, that will lead to an attempt to add its neighbor, node 10, which will lead to an attempt to add node 9, and so on; in this case, infinitely, because there happens to be a cycle in the graph.  But those writes aren't doing anything; they're no-op writes, because when you take the union of a set and something already in it, you just get that set.  So the LVar is done changing; all meaningful writes are over with, and it would be safe to terminate if only we had a way to know that the meaningful writes are over with.

The way we accomplish this with LVars is using what we call *events* and *event handlers*.  We say that an *event* is a write that actually changes an LVar's state, as opposed to a no-op write, and an *event handler* is associated with an LVar and listens for state-changing updates to that LVar -- such as a new node arriving in the set of seen nodes.  The event handler can run a callback in reaction to that event (and a callback can do writes of its own, which can trigger further events, recursively).

Let's try writing our graph traversal function using event handlers.  To traverse a graph g starting from a particular node and find all the reachable nodes, we can first create a new LVar of a set type, called `seen`.   Then we attach an event handler to the `seen` set.  This `newHandler` function takes an LVar, and it takes the function that we want to run every time an event arrives, that is, every time a new node is added to the set.  Whenever that happens, the handler reacts by taking that newly arrived node, looking up its neighbors in the graph, and then mapping the `insert` function over the list of neighbors.  Then we add the starting node to the `seen` set, and we're off.  The event handler does the rest.

(Furthermore, it doesn't matter *when* you attach an event handler to an LVar.  If you attach it after some writes have already been done, then the callback will still run once for every state that's at or below the LVar's current state in the lattice.  So attaching a handler doesn't race with writes.)

How do you know when you're done handling events?  Along with handlers, we give you a `quiesce` operation that will block until all of the callbacks that were launched by a handler are done running.  So we call `quiesce` on the handler, which will block until we've handled all the meaningful writes.

But, now that we've quiesced, we still have a problem: how do we *look at the exact contents* of the LVar that just quiesced?  We can't do it with an ordinary threshold read.  So there's one last piece of the LVars model to introduce, and it's what we call freezing.

## (freezing and quasi-determinism)

In addition to the usual threshold reads, we have an operation called `freeze`.  The `freeze` operation is a non-blocking read that lets us find out the exact contents of an LVar.  

Importantly, though, once an LVar has been frozen, its state can't change anymore, and any further writes that would change its state instead raise an exception.

So it's a good thing that we quiesced before we froze, so we know that no more writes to the LVar are going to occur, making it safe to look at the LVar's exact contents.

Now, earlier I complained that one problem with my original shopping cart code, once I'd corrected it to have all the synchronization it needed to have, was that I actually had to write all those synchronization barriers myself, and if I got it wrong I'd be back to nondeterminism.  Aren't we just back in the same position now, now that we have to make sure to quiesce before we freeze?  Aren't we just relying on wait barriers being in the right places again?

The answer is no!  The reason why is that *even if we forget to quiesce* or even if we do it in the wrong place, the *worst* thing that can happen by freezing too early is that we raise a write-after-freeze exception.  For a program that performs freezes, we can guarantee that there are only two possible outcomes. All executions that produce a final value produce the *same* final value, but some executions might raise an exception.

Formally, our determinism result is that if we have two executions, starting from a particular program configuration sigma, and those two executions end, then either the ending configurations are the same, or one or the other of the ending configurations is the error configuration.

So, it's not possible to get multiple observable outcomes from the same program, like we did with our first buggy shopping cart program, and this is true even if we forget to quiesce.  And, if we do hit the error case, it always means that there's a synchronization bug in our program, and in principle the error message can even tell us exactly which write happened after which freeze, so we can more easily debug it.

Now, this quasi-determinism result is nice, but what does it actually mean for us writing programs?

### Don't have to get into this unless someone asks

"How do you make sure not to stop until all threads are done?"

We have a work-stealing scheduler, and it uses a concurrent double-ended queue to store units of work.  Whenever threads launch, they're added to the queue of work that can be stolen.  The main thread isn't allowed to return unless there's no work left in the queue.

"What about termination?"

Yes, it's true that both runs have to end.  However I want to point out that they don't have to end in a value.  They just need to have reached a configuration where they can't take a step.  So, a program could deadlock, but it'll deadlock quasi-deterministically!

But as for divergence, we thought about this a while ago, and I believe we figured out that it's possible to construct a program where one run ends in error and the other one diverges.  My conjecture is that if one run ends in a non-error result, then we should be able to show that the other one won't diverge, but we don't have proof of that.

"Since we need to quiesce before freezes, why not just make quiescence a part of freezing?"

The quiesce and freeze operations apply to different things.  `freeze` applies to an LVar, whereas `quiesce` applies to something called a handler pool, which is the is the context that a group of callbacks are running in.

It happened to be in this program that there was a one-to-one relationship, one LVar and one handler pool, but that isn't always the case.

So if you want to have a combined freeze and quiesce operation, it would have to take that handler pool as an argument as well.  And a freeze that just takes an LVar argument is the most general thing because sometimes you want to freeze something even when no handlers are involved -- like with the shopping cart, maybe you want to freeze and return the cart contents, but there were no handlers involved.  Having said that, in the LVish calculus for which we prove determinism there are actually two freezes: there's simple freeze that just takes an LVar argument, and there's a construct called "freeze-after" that combines registering a handler with an LVar, starting the computation off (say, by inserting that first starting node in the set), quiescing the computation, and finally freezing it.  And in our library we actually provide a bunch of combinators that bring together these different pieces.  So the way I wrote it here, with explicitly quiescing and then freezing -- I wouldn't have had to do it that way.

## (roadmap)

[TODO: figure out what to say here -- something about the part of the proof that I'm going to talk about.]

## (proof)

[TODO: revise this; probably focus on determinism]

So how do we prove this property?  Well, the key to getting to quasi-determinism is that we prove a local confluence lemma, which says that if we start from that starting configuration sigma and we can step in two different ways from it, then there exists some third configuration sigma_c that those two, sigma_a and sigma_b, can each step to in no more than one step.  Actually, this property is really "local quasi-confluence" which means that either this is true, or one of sigma_a and sigma_b steps to error.  I've actually left out all the error cases in this picture, both for quasi-determinism and for quasi-confluence, to keep it simple, but the error cases are actually straightforward, and this case is where all the action is.

Now, once we have this local quasi-confluence thing, it's easy to get to quasi-determinism, by induction on the number of steps.  But how do we get to local quasi-confluence?

Well, it turns out that the particularly interesting case of quasi-confluence is the one in which different writes have happened on each side.  So the store in each configuration is changing, too, and so in sigma_a and sigma_b we might have two different stores.  And somehow we have to figure out how to put those separately updated stores together to get a single sigma_c.

So do do that we needed to prove a property that we call "Independence" that captures that idea that independent effects commute with each other.  And the way to read this is that if the thing above the line is true, then the thing below the line is true.  So in this case, Independence says that if we have a program configuration of a store S and an expression e and that steps to some other configuration S-prime, e-prime, then we know the following.  We can take some third store, S-double-prime, and combine it with our store, using this least-upper-bound-S operation, which is just the operation that combines all the LVars that are in both stores, taking the least upper bound of any that both wrote to.  And what are we going to get out when we step this expression e in that bigger store?  Well, stepping e will land us in the join of S-prime and S-double-prime, and e itself will step to e-prime, just like it did before.

In the real proof, there are some side conditions on what S-double-prime can be, in particular to rule out interference from freezing.  But this is what's going on at a high level.

### (more detail if anyone asks)

We're starting from a given program configuration, which is just a program expression and a memory store where LVars can be allocated.  As part of the definition of the language we define this arrow evaluation relation that describes how configurations get evaluated; basically, that expression reduces, and the store grows.  And this continues until the expression has reached a value or until no more reductions can take place.  And the property we want is quasi-determinism, which is just that if we apply that reduction relation repeatedly to a configuration until it can't be applied anymore, and then we do it again, then we'll end up with the same thing, both times.  Note that we're *not* claiming to go through the same series of reduction steps on both runs; just that the *outcome* is the same.  And the other possibility is that one or the other of the configurations will end up in what we call the error configuration, which is how we model exceptions, but I've left that out of the diagram here to keep it simple.

So how do we prove this property?  Well, the key to getting to quasi-determinism is that we prove a local confluence lemma, which says that if we start from that starting configuration sigma and we can step in two different ways from it, then there exists some third configuration sigma_c that those two, sigma_a and sigma_b, can each step to in no more than one step.  Actually, this property is really "local quasi-confluence" which means that either this is true, or one of sigma_a and sigma_b steps to error.  I've also left out the error case here to keep it simple, but the error case is straightforward.

Now, once we have this local quasi-confluence thing, it's easy to get to quasi-determinism, by induction on the number of steps.  But how do we get to local quasi-confluence?

It turns out that there's one particularly interesting case.  Imagine if the expression in that configuration sigma has two subexpressions in it that can take a step.  So that's how we might end up at a different sigma_a and sigma_b, by different parts of the expression stepping.  But expressions are also allowed to allocate LVars, and write to LVars.  So the store in each configuration is changing, too, and so in sigma_a and sigma_b we might have two different stores.  And somehow we have to figure out how to put those separately updated stores together to get a single sigma_c.

So do do that we needed to prove a property that we call "Independence" that captures that idea that independent effects commute with each other.  And the way to read this is that if the thing above the line is true, then the thing below the line is true.  So in this case, Independence says that if we have a program configuration of a store S and an expression e and that steps to some other configuration S-prime, e-prime, then we know the following.  We can take some third store, S-double-prime, and combine it with our store, using this least-upper-bound-S operation, which is just the operation that combines all the LVars that are in both stores, taking the least upper bound of any that both wrote to.  And what are we going to get out when we step this expression e in that bigger store?  Well, stepping e will land us in the join of S-prime and S-double-prime, and e itself will step to e-prime, just like it did before.

In the real proof, there are some side conditions on what S-double-prime can be, in particular to rule out interference from freezing.  But this is what's going on at a high level.

## (frame rule)

[TODO: revise this]

So, to give an idea of why this independence property we had to show is interesting, I put it up here next to what's known as the frame property, or frame rule, from separation logic, which is one of the really cool developments in program verification over the last fifteen years or so.

What the frame rule says is that, suppose you have some precondition p, and then you run a command, c, and then postcondition q is true afterward.  If you know that to be true, then you can add on this extra precondition, r, which is separated by this star, which is called "separating conjunction", and means they're both true but *disjoint* with each other -- for example, p might describe the state of one part of memory and r might describe the state of some disjoint part of memory.  Then, the frame rule says that if you run c, then not only will you still end up with q true, but you'll end up with r still true as well.  So, in other words, you get this two-way non-interference guarantee: if this r thing is outside of the memory footprint of c, then not only will running c not interfere with r, but having r there won't interfere with the running of c either.

The independence property that we proved is like a frame rule, except that instead of that separating conjunction, where the pieces have to be disjoint, now the pieces are actually allowed to overlap, and instead of separating conjunction we have least upper-bound!  So this S and S-double-prime can actually overlap, but we can still *reason* about them as if they were separate, and in our case the double-edged guarantee is that, first of all, running this expression in a bigger store will still update the store to something predictable, namely S-prime joined with S-double-prime, and not S-prime joined with some other weird thing, and second of all, having S-double-prime in there won't interfere with e, e will still step to e-prime just like always.

So we can use this nice idea of non-interference of disjoint memory, and use it to reason about things that actually do overlap in memory but still don't interfere.  And I think this is cool because it raises interesting possibilities for how LVars fit into a whole galaxy of work in program verification, and that could be a whole talk on its own!

## (freeze after writing)

Well, the good news is that this particular graph traversal is deterministic.  The bad news is that, in general, freezing introduces quasi-determinism to the programming model, because we might freeze before we've quiesced.  Or, our thread that's freezing and quiescing might be racing with some other  thread that's racing to write into the same LVar.

But, if we can ensure that freezes only ever happen after all writes have completed, then our computations should be deterministic, because we won't have any write-after-freeze races.  Well, it turns out that there's actually a way to enforce this at the implementation level.  And the trick to that is to just let the programming model take care of the quiesce and freeze at the end of a computation for us.  We call this mechanism `runParThenFreeze`, and that finally brings me to our Haskell library for programming with LVars, which we couldn't resist calling...

## (the LVish library)

..."LVish"!  All of the code examples I've been showing are written with the LVish library.

When you're using LVish, you write what are called `Par` computations, short for "parallel", and and it's only inside a `Par` computation that LVar operations are allowed to run.  When you run a `Par` computation, it's dynamically scheduled by a work-stealing scheduler that's part of our library.  The scheduler implementation is interesting in its own right, and we discuss it more in our paper, but for now all I want to say is that these are lightweight, library-level threads; we didn't have to change anything about the underlying run-time system.

Nothing about the LVars model is specific to Haskell, but because we did implement it in Haskell we were able to do some fun tricks.  One of those is that `Par` computations are parameterized by an *effect level*, which allows for fine-grained specification of the effects that a given computation is allowed to perform.  The effect level is the first type parameter to the Par type.  So, for instance, this computation is indexed with a `Det` effect level, for deterministic.  And because of that, if I tried to do a `freeze` inside this computation, it would raise a type error.  So, the *type* of an LVish computation reflects its determinism or quasi-determinism guarantee.

And the other cool trick is this `runParThenFreeze` operation that LVish provides, which runs a `Par` computation and then returns a frozen LVar at the end of it, after an implicit global barrier.  When you run an LVish computation with `runParThenFreeze`, then you don't have to manually `freeze` the LVar yourself, which means you don't have to manually `quiesce`, either.  So, for instance, this is a `Par` computation that only does writes and then returns, so it can have a deterministic effect level, but if we run that computation with `runParThenFreeze`, and we'll have a deterministic program that always shows us the complete contents of the cart, because of that implicit barrier.

The LVish library also provides a bunch of data structures: IVars, IStructures, sets, maps, and so on, including some efficient lock-free implementations of sets and maps.  And, in addition to the provided ones, users can implement their own LVar types, and the library provides some handy stuff to facilitate that.

And you can install it today!

## (deterministic parallel programming)

Now, the goal of parallel programming is to write programs in such a way that they can be scheduled onto multiple cores in order to make them go faster.  And, so far I've been talking about determinism, but all of this is for naught unless we actually *can* make programs go faster!  So let's see how we're doing on that.

First I want to mention that what we want out of deterministic parallel programming models in general is that they produce predictable results, in spite of the unpredictable ways in which parallel tasks are scheduled.  Now, there are some parallel algorithms for which this isn't such a big deal, because you can actually predict scheduling in advance.  If you're doing something like, say, a data-parallel operation on every entry in a thousand-by-a-thousand matrix, then you have a million independent parallel tasks, and you just divide those million tasks evenly by the number of processors you've got.  But it's harder if the parallel algorithm you want to implement is a so-called "irregular" parallel algorithm, where the amount of potential parallelism in the problem depends on the input.  Take our parallel graph traversal, for instance.  How much potential parallelism is there to exploit in that problem?  Well, we have no idea in advance.  We don't find out until we're actually in the graph, stepping through it, seeing what the outdegree of nodes are and launching tasks to handle the nodes they point to.  So, for that kind of problem, because we don't even know how many tasks we'll have to do until we're doing them, they have to be dynamically scheduled.

So that's the kind of problem that our dynamic work-stealing scheduler for LVish is capable of  doing really well on.  Well, it turns out that there are lots of problems like that, and as a case study, we used the LVish library to parallelize one of them.

## (k-CFA)

So this begins with a story.  A year and a half ago I was at a conference and I got into a conversation with my friend Matt Might about potential applications for LVars, and, he happens to be a static analysis researcher, so unsurprisingly, he said, "You should definitely apply LVars to static analysis."

So, Matt suggested that we could use LVars to parallelize a *k*-CFA static program analysis.  The idea of *k*-CFA is to explore a graph of abstract states of a program, with the goal of discovering the flow of control in the program without actually having to run it.  And, this is a picture of one of those abstract state transition graphs, showing the abstract states and the transitions between them, from one of his papers.  Now, if I recall correctly, this is for a toy 9-line program, so as you can imagine the state transition graphs for realistic programs are much larger.

Well, it turned out, once I started looking into it, that the algorithm relied on detecting quiescence in a fixpoint computation, just as in the simpler graph traversal that I showed before.   So, once we had support for quiescence and freezing, we were able to sit down and implement *k*-CFA in LVish.  In particular, what we did was port a pure Haskell version of the *k*-CFA analysis to a parallel version using LVish.

So at that point, we were able to run our parallel implementation of k-CFA on one of the benchmarks from that same paper. This is with *k* set to 2, by the way; in *k*-CFA, *k* is the length of the call history that's stored in each of these abstract states.

There are really three things that are interesting about these results.

First, the program we were parallelizing was a pure functional, idiomatic Haskell program.  There was a lot of potential for parallelism in it.  But functional programming didn't really serve them well because there was tons of allocation and copying data around.  So, just being able to have shared mutable data structures, even though they're only mutable in the extremely restricted and determinism-preserving way that LVish allows, that by itself gave us an up to 20x speedup, even just on one core.

Well, that was exciting, and then we turned on parallelization.  So here's what we saw.

We implemented a version of this benchmark called "blur" from the *k*-CFA paper that I mentioned.    The blue line here shows what a linear parallel speedup would look like as we add cores, and the green line is how we did.  So we ended up getting a little more than an 8x speedup on 12 cores.

The last thing to point out is that we actually implemented this algorithm using two different LVar data structures that the LVish library provides.  We did it with our lock-free concurrent set based on skip lists, and we also tried it  with our reference implementation of a set, which is basically just a pure Haskell data structure wrapped in an LVar, and that's what the yellow line is.

Now, yellow is normalized to the same baseline as green, for the single-core version.  Notice that it does a little better up to four cores or so, but then it stops scaling.

The really interesting point here is not that it helps to use a good, efficient, concurrent data structure, because although that's true, we already knew that.  The interesting point is that there's nothing about using LVish that precludes using these efficient concurrent data structures.  Anything that has the semantics of an LVar is fine for the determinism guarantee.  So you don't have to give up that guarantee if you want to use a fancy data structure.

We also did one other benchmark, and this is on a program called "notChain", which is just a long chain of 300 "not" functions, "not not not not" et cetera, which was particularly designed to negate the benefits of our sharing approach.  And, indeed, that one didn't get anything close to the 20x speedup from sharing; I don't think there was any speedup at all.

But in terms of parallelism, it did pretty similarly to the blur benchmark -- so, as you can see, the lock-free version managed to get a respectable speedup -- around 7x on 12 cores, and the version with the reference implementation of a set did about the same as the other benchmark did.

So we did succeed in our goal of making programs go faster.

[TODO: update this part]

And, finally, I don't have time to cover it in this talk, but we did another case study for parallelizing an existing Haskell program, in this case it was a bioinformatics application that did phylogenetic tree binning, which involved comparing the tree edit distance between every pair of trees in a set of hundreds or thousands of phylogenetic trees.  And we got a respectable speedup on this, too, and it's in our paper that's appearing at PLDI in June and I'm happy to talk more about it later if you want.

## (roadmap)

[TODO: figure out what to say here about distsys]

## (distributed systems)

[TODO: figure out what to say here]

## (eventual consistency)

One of the things that makes distributed programming hard is that you have data that's replicated across a number of physical locations.  So, as if it wasn't hard enough just to deal with our shopping cart when it's just running on my laptop, imagine if it's replicated in data centers around the world, as it almost certainly actually is.

Ideally, the system would be consistent, meaning that every replica always sees the same information, but in practice, it isn't going to be, because that goal is in tension with our desire for the system to be highly available, for both reading and writing.  And as if those weren't bad enough, we also have to deal with parts of the network catching on fire and being unable to talk to each other from time to time.

Well, the CAP theorem from distributed systems tells us that if we want to be robust to the inevitable network partitions, we have to compromise on at least one of consistency or availability.  And if we need to have high availability, we sacrifice strong consistency for what's known as eventual consistency, in which replicas may not always agree, and the only guarantee is that they'll eventually come to agree.

But that leaves us with another problem, which is, how are we going to get all replicas to agree?  If replicas differ, how do we know which one is right?

Well, there's been a lot of work done on this, but there's one particular quotation I wanted to share which...

## (Dynamo, CRDTs, joining forces)

...happens to be from the Dynamo paper, on Amazon's distributed key-value cloud storage system, and this quotation has to do with application-specific mechanisms for resolving conflicts between replicas.  The idea here is that, for instance, if you want to implement a shopping cart, and two replicas disagree on what's in the cart, then you can plug in a conflict resolution operation that combines them in some intelligent way, rather than just going with the one that wrote last.

The Dynamo paper doesn't mention lattices at all, but later on, Marc Shapiro and collaborators came up with what they called conflict-free replicated data types, or CRDTs.  These are based on the idea that, if we can define an lattice for the states that an object in the data store can be in, then replicas of that object that differ can merge with one another in a deterministic way, because that application-specific conflict resolution operation is just least upper bound.  Sounds familiar!

## (two styles of CRDTs)

[TODO: see if there's a cute illustration I can use]

[TODO: revise this; I probably don't need to go into detail about this.  Instead, just hit the high points of chapter 5.]

Now, as a brief digression, it turns out that there are two styles of specifying CRDTs.

The style I was just talking about is called the "convergent" or "state-based" style, and these are what are known as CvRDTs.  To specify a CvRDT, you specify the states that a replica can take on, and you specify a join operation on states, so that set of states together with that operation form a lattice, and a replica can converge with a remote replica by taking the join of its local state and the remote replica's state.

There's also another style, which is called the "commutative" or "operation-based" style, or "op-based", for short.  In the op-based style, there isn't necessarily a lattice, there's just a commutative operation, or family of operations, and a replica can converge with a remote replica not by asking for the remote replica's state but just by asking what operations have taken place on the remote replica.  Then it applies those operations to its own state, and because they're commutative, order doesn't matter.

The cool thing about these state-based and op-based styles is that they're equivalent -- if you have a CRDT specified in one style then there's a general way to construct one specified in the other style, and vice versa.  Having said that, in our work we just look at CvRDTs because they're the most obviously similar to LVars.

## (LVars vs. CvRDTs)

[TODO: revise this]

***THIS ONE NEEDS TO BE SCROLLED.***

OK, so how do LVars and CvRDTs differ?

The first difference is that when you read from an LVar, it's a threshold read, and so it's deterministic.  There is a way to read the exact contents of an LVar, but you can only do that after it's in a so-called "frozen" state and can no longer change.  Meanwhile, with a CvRDT, you can read its intermediate states, while it's still changing.  So queries of a CvRDT are not deterministic.  So that's the first difference -- and we like determinism, so we want to keep this property.

The second thing that's different is that when you write to an LVar, what ends up being written is the least upper bound of the old contents and the new contents.  So every write computes a join.  On the other hand, with CvRDTs, it's more general than that.  Every write has to be inflationary, that is, it has to move that CvRDT's state up in the lattice or leave it the same, but it doesn't necessarily have to be a least upper bound; it can be any kind of inflationary write.  The only time you do a *join* is when you merge the state of a remote replica with your local replica.  And we should be able to do this with LVars as well -- there's no reason why we should be limited to least-upper-bound writes.  So, we like general inflationary writes.

A third thing that's different, of course, is that LVars are shared-memory, and CvRDTs are distributed and replicated!  And there's really neither of these that's the winner; at least, for now, they seem to be good for different things.

But what we can do is bring the strong points of LVars to CvRDTs, and vice versa.  So here's what we propose.  First of all, we want to be able to do threshold reads of CvRDTs along with the ordinary reads that we can do now.  If we had both kinds of reads, then we could use the framework of lattices to reason about both eventually consistent and strongly consistent reads within the same system. And we think this is a good thing, since real systems allow one to make conistency choices at the level of individual queries, so our formalisms should be able to handle these per-query consistency choices, too.

The other thing we're proposing is adding general inflationary updates to LVars beyond least-upper-bound writes.  And this is frankly something that we should have done long ago, because we're already supporting this in our Haskell library, and it's important for applications that need things like monotonically incremented counters.  So we need to show that this is actually deterministic.  The challenge here is that the inflationary update operations you pick have to commute with each other, and this might mean that you can't do ordinary least-upper-bound anymore -- for instance, it doesn't commute with increment.  So you have to be careful with what you allow.  But we always had to make sure to pick a set of commutative write operations -- it's just that, before, that set had one thing in it, and it was least upper bound.  Now we're generalizing that.

## (landscape again)

[TODO: revise to bring up to date a bit]

So, to return to our landscape of deterministic parallel programming, where do LVars and LVish fit in?  Actually, they're all across this landscape.  We've got the pure functional core, on which Haskell is based, and so we can do this pure task parallelism.  We can do single-assignment programming, of course, because IVars are a special case of LVars, and I'll even put two checkmarks here to show that not only can we do this, we can generalize it.

And because LVars generalize to arbitrary lattices, we can use LVars to express Kahn process networks, because we can use LVars to represent a lattice of channel histories with a prefix ordering.  And, in addition to all this stuff we've discovered a new space of quasi-deterministic programs.

So all this is stuff you can do in the currently released LVish library of today.

And, the one thing left is disjoint imperative parallelism, but, as of recently, we've been able to fit that our model as well.  The trick to doing this is by applying what we call a StateT transformer to the Par monad that will let us thread some state through, and at a fork the state has to be either split or duplicated.

So if we're splitting a vector like this one, the Haskell type system is powerful enough to ensure at compile time that neither of the child computations can access the original complete vector.  And this is also in our PLDI paper, and it's in our not-yet-released version of LVish which we plan to release in the next couple months.

And, eventually, we'll have threshold-readable CvRDTs and we'll have distributed LVish, and in that case we'll have LVars not only across the landscape but also in the cloud!

## (final slide)

[TODO: figure out what I can say to sum up -- something about unifying abstractions!]

That's all I've got -- thank you very much, and I'm happy to take questions.
