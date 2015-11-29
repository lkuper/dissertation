> Referees' Comments to Author:
> Referee: 1

<!--

> Comments to the Author
> SUMMARY

> This paper presents a programming model that is parallel and
> deterministic-by-construction. It is based on LVars: mutable
> variables ranging over a join-semi-lattice. The operations
> permitted on an LVar are restricted so as to always commute:

>  - a put operation cannot write an arbitrary value;
>    instead, it computes the join of the previous value
>    and the new value; this implies that the value of an
>    LVar must evolve monotonically with time;

>  - a get operation cannot observe the current value;
>    instead, it blocks until a certain threshold has been
>    reached.

> This guarantees that, even though scheduling is non-deterministic, computation
> is deterministic, in the sense that its final result is always the same. This
> result is proved, in Section 2, in the setting of a concurrent call-by-value
> lambda-calculus, equipped with an interleaving semantics, where put and get
> are atomic operations.

-->

> The programming model is then extended, for greater expressiveness,
> with the following operations:

>  1 an update operation doesn't have to be of the form "join previous
>    value with new value"; an arbitrary family of update operations
>    can be allowed, as long as they are inflationary and commute with
>    each other. A counter equipped with an "increment" operation is a
>    typical example.

>  2 in order to allow least fixed point computations, a few (rather low-level)
>    operations are introduced, namely newPool, addHandlerInPool, and quiesce.

>  3 a freeze operation is introduced. After an LVar has been frozen,
>    normal reads are allowed, whereas updates are disallowed.
>    Attempting an update after freezing, is a (fatal) error, which is
>    detected at runtime. (I would assume the same is true of
>    attempting a normal read before freezing. But normal reads
>    do not seem to be explicitly modeled in the calculus.)
>    This makes the model quasi-deterministic, an interesting notion.

> As far as I understand, these three extensions are independent of one another,
> and only the last one causes the calculus to become quasi-deterministic, as
> opposed to deterministic. Unfortunately, this is not very clearly stated. In
> Section 3, the calculus is extended with feature 1 and with a restricted
> combination of features 2 and 3, and quasi-determinism is established: a
> computation may fail or succeed in a non-deterministic manner, but if it
> succeeds, then its final result is always the same. This is a modified version
> of the determinism theorem of Section 2.

The reviewer is right that arbitrary update, handlers/quiescence, and
freezing are independent from each other and it is only freezing that
actually introduces any nondeterminism.

<--

> This programming model has been implemented in Haskell as a library, called
> LVish. The library is presented informally in Section 4. Its API is not shown
> explicitly; instead, a few examples of use are shown. Part of the
> implementation is shown. A couple case studies, "k-CFA" and "PhyBin" are
> discussed. It is argued that user programs can be written in a declarative
> style, with a (quasi?)determinism guarantee, and yet exploit sophisticated
> concurrent data structures under the hood and (therefore) exhibit good scaling
> behavior.

> EVALUATION : PROS

> I like the overall idea behind LVars. There is clearly an interesting and
> elegant programming model here. It is declarative in style, deterministic or
> quasi-deterministic, yet potentially allows exploiting parallel hardware.

-->

> The manner in which this model subsumes Kahn networks and IVars is
> interesting. (As far as I understand, the claim that LVars subsume Kahn
> networks is of a rather theoretical nature, since in practice, a Kahn network
> would be implemented in a distributed setting, using asynchronous message
> passing, without storing channel histories; I don't see how it could/would be
> implemented on top of LVars.)

The reviewer has a fair point about the claim about Kahn networks
being theoretical.  We should be forthcoming about this.

<!--

> Regardless of whether the reported parallel speedups are convincing, this
> paper reports on an impressive amount of work, which deserves being recorded
> and published. (As far as I understand, the speedups reported in Section 4
> compare the parallel version of the code run on 12 cores versus the same code
> run on 1 core. They do not compare it against a sequential version of the
> code, which seems a regrettable omission.)

> EVALUATION : CONS, CRITICISMS, SUGGESTIONS, etc.

-->

> One may argue that the results that *are* proven in the paper are relatively
> easy / intuitively clear: that is, provided updates are inflationary and
> commute with each other, and provided normal "reads" are replaced with
> blocking "gets", then computation is deterministic. (This is proved based on
> an interleaving semantics, where puts and gets are atomic. The proof is based
> on a local confluence lemma.) The proof is done twice, once in a simpler
> deterministic setting, once in a more complex quasi-deterministic setting, but
> its overall structure and intuition seems to be the same. The result that is
> *not* proven in the paper, however, is that the low-level Haskell code is a
> valid implementation with respect to the high-level interleaving semantics.
> This is far from obvious, since the low-level code involves non-atomic
> implementations of putLV and getLV, race conditions, user-supplied concurrent
> (possibly lock-free) data structures, an unspecified memory model, etc. This
> is a little bit frustrating -- although I understand that proving the validity
> of the low-level implementation would probably warrant another paper.

The reviewer is right that we don't tackle this question.  Indeed, the
correspondence between the LVish Haskell library and the lambdaLVish
calculus is loose.  Actually proving that the low-level scheduler code
is valid with respect to the paper semantics, though, would probably
require not just *one* more paper.  It would more likely take years of
work.

> The duplication between the two lambda-calculi, lambda_LVar and lambda_LVish,
> bothers me. The basic intuitions seem to be the same, and the series of lemmas
> seems to be roughly the same. Is it really necessary/useful to take the reader
> twice through these steps? I understand that lambda_LVish adds extra details
> and complications (such as the frozen bits) whereas lambda_LVar is more basic
> and abstract, hence easier to understand at first. I wonder if one could merge
> the two calculi so as to obtain the best of both worlds at once: the abstract
> elegance of lambda_LVar, and the expressive power of lambda_LVish. This could
> be achieved, I believe (perhaps naively), by viewing "freeze" as just another
> update operation.

I think it would be very hard to have our cake and eat it too, here.
The main thing that complicates the definition of lambdaLVish is the
addition of handlers/quiescence, and that's also the thing that makes
it more expressive.

What we've done to simplify matters, though, is add arbitrary update
operations to the original calculus, lambdaLVar.  This makes
lambdaLVar a tiny bit more complicated, since we have to parameterize
the calculus by U.  But it makes for a smaller leap from lambdaLVar to
lambdaLVish, and it demonstrate that the arbitrary update operations
in lambdaLVish are not the source of nondeterminism.

The way things are set up in the paper now are something of an
accident of history.  When we developed LVars, we first came up with
lambdaLVar (a version of which first appeared in FHPC '13); then we
came up with the addition of handlers/quiescence and freezing (POPL
'14); and then, finally, later, we added arbitrary update (which it
turned out was necessary to model a number of real problems, but which
was never written up until now, although we discussed it informally in
our PLDI '14 paper).  When we finally added arbitrary update and wrote
up the quasi-determinism proof, it was lambdaLVish that we added it
to, because lambdaLVish was the most recent iteration of the calculus.
But it makes sense pedagogically and technically to have it added to
lambdaLVar.

It also makes sense from a language-design perspective to add
handlers/quiescence and freezing (and nothing else) to the model at
the same time, because handlers/quiescence and freezing are two
features that are especially useful in combination.  So the revised
version of the paper has two calculi: lambdaLVar with arbitrary
update, and lambdaLVish, which adds handlers/quiescence and freezing
on top of that.  We are still not adding handlers/quiescence in
isolation and proving that that feature does not by itself introduce
nondeterminism, but we want to spare dragging the reader through yet
another intermediate step.

Viewing freeze as just another update operation is an interesting
idea.  But, unlike other update operations, freeze returns a value.
Moreover, since it's the one feature that introduces nondeterminism,
we think it makes sense to separate it.

Presentation-wise, we agree with the reviewer that it's not necessary
to go through all the steps of the proof twice, once for lambdaLVar
and once for lambdaLVish.  We've made section 3 less repetitive. [TODO]

> If D is the lattice chosen by the end user, then we can work
> in the lattice D_p (as defined on page 32). There, we find that "freeze" is
> inflationary (it moves up from (d, 0) to (d, 1)) and quasi-commutes with every
> update operation (i.e., either the operations actually commute, or one
> execution fails). In summary, it seems that one could establish
> quasi-determinism in a nice abstract setting where "freeze" and the "frozen"
> bits do not explicitly appear, and then find that they can be obtained as a
> special case of the abstract framework. Also, perhaps, in this more abstract
> setting, one would see that other operations are permitted: for example, an
> operation that blocks until an LVar is frozen would be just a special case of
> "get", hence clearly permitted, whereas currently it does not seem to exist.

In fact, it *is* currently possible to write an operation that blocks
until an LVar is frozen, because the semantics exposes the status bits
that tell you whether an LVar is frozen or not.  An example in PLT
Redex is here:
https://github.com/lkuper/lvar-semantics/blob/f1f3597299bcf9f47f87d60845bbc8d168387a84/lambdaLVish/nat.rkt#L625.
More generally, the threshold set could be { (d, frz) | frz = true }.
It's a perfectly fine threshold set, since any two frozen states have
Top as their lub.

> Concerning "freeze/after/with", I am not sure why the authors choose to
> combine many independent operations of LVish into just one construct. Could
> one at least take "freeze" out of the picture and replace this construct with
> something along the lines of "quiesce/after/with", which performs the fixed
> point computation, but does not perform the final freeze? Thus, "freeze" would
> remain an independent construct, and would be seen more clearly as the only
> source of quasi-determinism. (Am I correct that it is?) Could one go further
> and formalize newPool, addHandlerInPool and quiesce as three separate
> operations? Would it be difficult? This is not discussed.

Yes, the reviewer is correct that freeze is the only source of
quasi-determinism.

It's true that it's a little awkward to have
handlers/quiescence/freezing rolled into one operation.  We do this
because it is how we intend handlers, quiescence, and freezing to be
used.  The LVish API in fact provides just such a freezeAfterWith
operation, but it also provides the individual building blocks.

<!--

> Concerning handlers and pools, one would like to know whether they are just "a
> mechanism for (parallel) least fixed point computations". Are they expressive
> enough to compute the least fixed point of an arbitrary monotonic function f
> on an arbitrary lattice? (Or is this true only of atomistic lattices?)
> Conversely, can they be used for any purpose other than computing the least
> fixed point of some monotonic function? What connections are there with the
> known sequential algorithms for efficiently computing least fixed points? (See
> references below.)

> The calculi lambda_LVar and lambda_LVish are equipped with an operational
> semantics, with interleaving, which has a very operational feel (obviously),
> especially concerning pools and handlers. Would it be possible to a propose a
> reference semantics that is much more declarative in flavor, perhaps a
> denotational semantics? where hopefully it would be clear *what* pools and
> handlers compute -- namely, a least fixed point, I suppose -- as opposed to
> *how* they compute it.

> The authors go to the trouble of defining calculi where dynamic allocation of
> LVars is supported. This creates some complication (e.g., one must reason up
> to renamings of memory locations) compared to some related work where a fixed
> collection of LVars is assumed. This is good; I like the fact that the authors
> have attempted to define a nontrivial calculus. That said, they have omitted a
> feature which seems to be important in the case studies, namely "higher-order
> LVars", the ability for an LVar to contain a pointer to another LVar. In the
> PhyBin case study, for instance, the splits map is a map of bipartitions to
> sets of trees, where both the "map" and the "sets" are stored in LVars. The
> calculi in Sections 2 and 3 cannot model this, since they assume a fixed
> lattice D, and apparently a memory location cannot be an element of D. (For
> one thing, elements of D are never subjected to renamings, it seems.) Could
> this restriction be easily relaxed?

Not much gets past this reviewer!  Yeah, we need to talk about this.
I don't remember exactly why we punted on allowing LVars to contain
pointers to LVars.  Was it because it would have complicated the
renaming story too much?  It does mean that there are fewer realistic
things that we can model; I think that a number of real use cases do
require nested LVars. I'm not sure what should be done about this,
really.  I don't want to add more complicated machinery around
renaming.

-->

> I find the description of the LVish API (Sections 4.1-4.3) to be really
> unsatisfactory. In fact, this description is absent. Examples of use are
> given, but a list of the types and operations provided by the library is
> missing. It is difficult to reconstruct this list based on the examples. For
> instance, I was not able to know for sure what is the type of runPar, or
> runParQuasiDet, or runParThenFreeze. I was quite thoroughly confused in
> Section 4.3, where it was not clear to me whether there is a single type
> called "Par" (that would be *the* Par monad) or a family of types (which one
> might call "Par", or otherwise) which are instances of the type class
> "ParMonad" (each member of this family would be *a* Par monad). In my opinion,
> the name "Par" is used in too many ways. Adding to my confusion, in the
> implementation section, the type "Par" is used sometimes with two parameters
> (as in "Par lvl a") and sometimes with three parameters (as in "Par s e ()").
> In summary, I think the API (or a fragment of it) must be explicitly given,
> and the "Par" confusion must be cleared up.

This part of the paper has changed since it was submitted for review.
What the reviewer saw was indeed pretty messy.  I think what's in
there now is much nicer.  It is not, however, a systematic
presentation of the LVish API.  It's merely a cleaner and
better-explained series of examples.

I was going to argue that the right place for a systematic
presentation of the LVish API is not this paper, but rather, the LVish
package documentation -- but on second thought, I think we should in
fact document the key stuff here.  (Besides, if we're going to leave
out the LVish internals stuff, we should at least do this, to throw
this poor reviewer a bone!)

> I find the description of the LVish implementation (Section 4.4) to be rather
> unsatisfactory as well. I don't really understand which pre-existing libraries
> it relies upon (if any). What is Sched? Does the code rely on some
> pre-existing parallel flavor of Haskell? If so, which one? If not, how is
> parallelism achieved? How can lock-free data structures be implemented (in C,
> presumably?) and which memory model is assumed? Although the code seems
> well-written and clearly structured, it is nontrivial, and the text that
> accompanies it is not really helpful (in my non-expert opinion). Also, not all
> of the code is shown; e.g., the code for addHandler is missing. I would
> suggest showing the missing code, and trying to improve the explanations.

We agree with the reviewer that this section of the originally
submitted paper was hard to understand.  Our proposed solution is to
fix the problem by simply leaving out the details of the LVish
internals.  For one thing, the code presented there is already out of
date, and the implementation is likely to continue to evolve.  It
doesn't do anyone any good to simply repeat the now-outdated
implementation section that appeared in our POPL '14 paper.  (For
example, the material in that paper about leveraging idempotency is
out of date, now that we allow non-idempotent updates.)  We believe
the best place to go to read up-to-date LVish scheduler code is the
open-source LVish repository (which is linked from the paper), not the
paper itself.

<!--

> EVALUATION : SUMMARY

> I suggest that this work deserves being published eventually, but the paper
> should undergo a round of revisions first. Ideally, I would like to see:

>  1. Serious thought given to the suggestion of merging lambda_LVar and
>     lambda_LVish into just one calculus, so as to reduce duplication and
>     shorten the paper. The new calculus should be as simple as lambda_LVar and
>     as powerful as lambda_LVish. Is it possible? I would like to think so, but
>     perhaps I am being naive.

>  2. An explicit presentation of the LVish API, possibly at the cost of
>     consuming extra space;

>  3. A fuller presentation and a better explanation of the LVish implementation.

> MISCELLANEOUS COMMENTS (HIGH-LEVEL)

> Are there examples of "typical" mistakes that lead to a deadlock or livelock?
> Is it difficult, in theory and/or in practice, to convince oneself that a
> program has no deadlocks or livelocks?

Hmm.  I think the reviewer is asking this because we point out that
ensuring determinism is orthogonal to ensuring deadlock-freedom or
livelock-freedom.  I know that there is a lot of work on analysis and
verification tools for ensuring deadlock-freedom or livelock-freedom.
I don't know any of it particularly well.  We could insert a few
sentences referencing some of those works, as a way of saying, "Hey,
if this is what you want, you'd be better served by looking at these
other papers", but I confess I don't know what the important ones are.

-->

> The comparison with the frame rule in Figure 7 is interesting. In fact, one
> may wonder: what would be a suitable set of Hoare-logic-like rules for
> reasoning about LVish programs? I assume that assertions would have to be
> stable (i.e. it's OK to assert that an LVar contains at least d; not OK to
> assert that it contains exactly d, unless it has been frozen first) and that
> there would be a frame rule where * does not mean "disjoint" but denotes a
> kind of LUB. The paper `heap-monotonic typestate' comes to mind along these
> lines.

This is a good question; indeed, thinking about separation logic for
deterministic parallelism is what led us to develop LVish in the first
place.  It would be interesting to see how LVars might fit into, say,
the Views framework mentioned in section 5.  We've added a sentence
suggesting that as an avenue for future work. [TODO]

<!--

> In a sequential setting, there has been work on lattice-independent least
> fixed point algorithms. These algorithms are based on propagating differences
> until a fixed point is reached. See
> e.g. http://www2.in.tum.de/bib/files/Hofmann10Fixpoint.pdf and the references
> therein. Is it possible to draw a comparison with the lattice-generic
> algorithm in Figures 11-13? I feel intuitively that there are strong
> similarities. But, because getLV/putLV/freezeLV/quiesce are not explicitly
> presented as "a concurrent least fixed point computation algorithm", it is not
> clear whether the code in the paper solves the "same" problem as these earlier
> algorithms.

That's a good question, but then again, I'd like to argue for leaving
that code out of the paper entirely because it is now out of date.  We
could instead say something brief about the general shape of the LVish
scheduler code.  But someone who wants to read the code can look at
the open-source implementation of LVish.

-->

> The code in Figures 11-13 is tricky and interesting. How would you approach
> the task of proving its correctness? The last paragraph of the paper mentions
> "a dependently-typed programming language". I can see how dependent types
> could help express and verify certain properties of (immutable) data
> structures. I do not see how they can help reason about concurrency,
> relaxed memory, etc. Could one instead envision an approach based on
> a concurrent program logic? or on model-checking? ...

A dependently typed langauge could help ensure that the states of an
LVar data structure do in fact form a join-semilattice, since
Haskell's type system is not powerful enough to guarantee such a
property, at least not without a great deal of pain.  Here we were
*not* thinking about verifying properties of the *scheduler*
implementation, which seems like an even harder problem (and, as
mentioned above, we propose leaving the referred-to code out of the
paper entirely).  We've added a clarifying comment about what we think
the role of dependent types could be. [TODO]

<!--

> MISCELLANEOUS COMMENTS (AS I READ)

-->

> p.3, it is not entirely clear to me what difference there is between "no
> shared state parallelism" and "disjoint state parallelism". Apparently,
> in both cases, restrictions are imposed so that there are no data races.
> Do the two approaches differ in the restrictions that they use? Does one
> of them subsume the other?

Indeed, part of the point of all this work is to demonastrate that
these things aren't so different after all.  We see the difference as
largely a matter of user interface.  In no-shared-state parallelism,
you cannot write code where different threads modify the same data
structure concurrently; there's not any way to represent that in the
model.  With disjoint imperative parallelism, you can write code where
two threads modify the same data structure, and it is then up to the
system to do some analysis to ensure that they are modifying different
parts of it.

<!--

> p.3, as a non-Haskell-expert, I am not completely clear about the status of
> Haskell's primitive thunks (i.e., unevaluated computations) in a parallel
> extension of Haskell. As far as I understand, a thunk seems comparable to an
> IVar insofar as it is only written once, but can be shared by several threads
> and can be demanded several times (possibly concurrently). To what extent does
> this comparison make sense?

-->

> p.4, "until the LVar in question reaches a (unique) value". Not
> clear at this point why it should be unique. If Top is written
> to the LVar, then Top dominates every value in the threshold set.

If the threshold set contains Top, then Top has to be the *only* value
in the threshold set.  The reviewer's confusion is probably because we
haven't explained yet that threshold sets have to be pairwise
incompatible.  But we have a forward reference to section 2, where
that is explained.

> p.5, "if we can ensure [...]" and p.6, "if a freeze is guaranteed to be the
> last effect [...]". It is not very clear at this point in which situations
> this static guarantee holds. Maybe a little bit of extra intuition and/or a
> forward pointer to the place where runParThenFreeze is explained would help.

There was such a forward pointer at the end of section 1.3, but it
makes more sense to put it in 1.4; we've moved it.  Thanks!

> p.7, not sure what the "connected component containing a vertex v" means in a
> directed graph. Is it the set of vertices reachable from v? the set of
> vertices that can reach v? or their intersection, i.e., the strongly connected
> component of v? Or (as suggested by the following paragraph) are you looking
> only for the set of "[vertices] connected to v"? (the successors of v?)

Yes, mean the set of vertices that's reachable from v.  We've tweaked
the wording in this section.

<!--

> p.8, "Streams [...] impose an excessively strict ordering". I am not sure in
> what way you are proposing that streams be used here. (Hence, no idea why it
> would be excessively strict.)

> p.8, "We could manually push [...] but doing so just passes the problem to the
> downstream consumer [...]". 1- I am not sure exactly how analyze would be
> pushed inside bf_traverse (I suppose parMap analyze would be called at every
> level?) and 2- I don't know what you mean in the second half of the sentence.

> p.9, "Moreover, we would still have to contend with the problem [...]". Not
> sure what problem you mean. Could you perhaps provide a more precise pointer
> into Marlow et al.'s paper?

> p.10, "then Example 2.1 will deterministically raise an exception". This is
> the first time you indicate that reaching Top causes an exception to be
> raised. (I guess this explains my problem with "(unique)" noted above.) Why is
> this a natural design choice? It seems that another viable choice would be to
> let the LVar silently reach Top and let the programmer explicitly check
> whether Top has been reached. But (if I understand correctly) it would then no
> longer be possible for get to return the unique threshold value that has been
> reached.

> p.13, "The definition of lambda_LVar is parameterized by D". One would expect
> different LVars to have values in different domains. But perhaps you are
> implicitly taking D to be the coalesced sum of all these domains? -- Footnote
> 6 confirms this, but comes a little late.

-->

> p.14, "the Top state is unreachable": unless one writes Top to the LVar. I
> don't think you have explicitly indicated yet that this operation is
> disallowed.

This is a good point; one is allowed to explicitly write Top to the
LVar (although by doing so, one would cause the program to step to the
error state right away).  What we really mean here is that one cannot
*unintentionally* reach Top.  We've reworded this.

> Definition 2.3, lub of stores: I would be interesting to know where this
> definition is used. Apparently not in the operational semantics (good). I am
> curious about some details (e.g. why you allow taking the lub of two stores
> with distinct domains) but cannot tell why this is reasonable/required if I
> don't know where this notion is used.

We use this definition in parts of the proof of determinism; in the
Independence lemma, for example.

> Definition 2.4, equality of stores: it seems to be "just equality". Is this
> definition really needed?

It is not -- and we don't actually use it anywhere anyway.  Removed;
thanks.

<--

> Lemma 2.2, I would expect the conclusion of the lemma to also state, "and
> pi(sigma) = sigma". Is this not needed?

> p.25, "It would be desirable to have a consistent termination property [...]".
> Even in the presence of this property, it seems that your results still would
> not completely characterize the behavior of programs with livelocks. As soon
> as a program has a livelock (which I take to mean, a thread that keeps stepping
> but never terminates), then neither Theorem 2.1 nor the consistent termination
> property have anything to say about this program. Maybe this is not a problem
> because nothing can be observed about this program?

> p.25, "We conjecture (but do not prove)": do you have any idea whether it
> would be difficult to prove?

> p.25, "it is possible to construct a program that can terminate in error on
> some runs and diverge on others". This sounds like a rather undesirable
> property. Could you clarify why it holds and whether one could think of a way
> of avoiding this problem? (I suppose it cannot really be helped, as it
> suffices to run an infinite loop in parallel with an error in order to obtain
> this behavior.)

> p.26, "we generalize the model as follows". I suggest that, instead of
> thinking in terms of a lattice and set of "update operations" that satisfy
> "two conditions", one could think in terms of a commutative monoid. The monoid
> elements would be the update operations. Applying an update would be the
> monoid's multiplication. Your first condition would be just the standard
> definition of ordering in a monoid (i.e. the lattice derives from the monoid),
> and your second condition would be just the commutativity of the monoid.
> Nothing deep, but perhaps it would be more enlightening to think of an LVar as
> ranging over a commutative monoid, as opposed to ranging over a lattice. After
> all, in the example of the atomic counter, the commutative monoid is clearly
> (nat, +).

> p.26, "an LVar could represent [...] a collection of LVars". Technically,
> your formalization does not cover "higher-order" LVars, since D is a fixed
> domain and does not contain memory locations...

> p.30, the idiom in section 3.1.2 (newPool/addHandlerInPool/quiesce) is a kind
> of fixed point computation over a group of LVars which together form a pool.
> Could this idiom be restricted (if necessary) so as to be well-behaved, and
> presented as a higher-level fixed point combinator? I am thinking, for
> instance, of requiring that the LVars associated with the pool h be created
> fresh at the same time as h. One could then hope that these LVars cannot be
> written by another thread and (as a result) quiescence, once obtained, would
> be permanent instead of transient. (That said, I am afraid that this idea does
> not work; one would need a way of forbidding the LVars from escaping.)

> p.30, "quiesce is almost always used together with freezing". Do you have
> examples where quiesce is *not* followed with freezing?

-->

> p.31, it is not clear to me how freezeSetAfter differs from freezeAfter.
> The one difference I can see is that the parameter Q has disappeared and
> has presumably been instantiated to the infinite set of all singletons.

We've revised this example so that it no longer uses freezeSetAfter.

> p.31, is it OK to freeze an already-frozen LVar?

Yes, it is -- freezing a frozen LVar is a no-op.

> p.34, Definition 3.4 seems to indicate that a "write" (an operation) applied
> to a frozen LVar does *not* cause an error if the operation does not affect
> the LVar. Why this decision? Wouldn't it be more natural to consider that any
> operation performed after a freeze is an error?

One way to look at it is this: because the operation does not change
the contents of the LVar, the contents of the frozen LVar are the same
as they would have been had the operation already happened before the
freeze.  So, when the operation happens, it's fine to "pretend that it
already happened" and simply ignore it.  Raising an error would
introduce unnecessary nondeterminism.

<!--

> p.35, it seems a pity that a lot of definitions and lemmas have to be
> duplicated for lambda_LVar and lambda_LVish. One wonders whether one could
> generalize slightly the initial presentation of lambda_LVar so that the
> introduction of the "frozen" bits in the store could be viewed simply as an
> instantiation. E.g. if the initial framework allowed D to be a
> "quasi-commutative monoid" (where x.y = y.x holds, unless one of the two is
> Top) (i.e., exchanging two operations either has no effect or causes a global
> failure) then perhaps one could later instantiate D with the desired set of
> operations (update, freeze) without needing to modify the abstract framework?
> One would prove abstractly (and just once) that if the monoid is commutative
> then the calculus is deterministic and if the monoid is quasi-commutative then
> the calculus is quasi-deterministic.

-->

> Figure 8. I thought the point of introducing "freezing" was to allow normal
> (non-blocking) reads. But I don't see a new "normal read" operation in the
> syntax of lambda_LVish. Was it omitted from the calculus? Or is there no such
> operation in LVish, perhaps because it is built into combinators such as
> runParThenFreeze?

The freeze operation is itself a read operation.

<!--

> p.37, "freeze-after-with" seems to be a rather ad-hoc construct. One wonders
> whether it could be replaced (at least in theory) with something much simpler.
> Roughly speaking, "freeze l after Q with \x.e" seems to mean:

>   for every d in Q,
>     asynchronously spawn the thread:
>       let x = get l d in e;
>   freeze l

> This semantics is more relaxed (it may attempt to freeze l before quiescence
> has been reached), but allowing too many executions is not a problem for the
> purpose of establishing quasi-determinism. What I am suggesting is, perhaps
> the proof of quasi-determinism could be carried out in lambda_LVar extended
> with just "freeze" and "asynchronous spawn". Perhaps the proof would then
> become sufficiently simple so as to warrant fusing the calculi lambda_LVar and
> lambda_LVish into just one calculus.

> p.49, "e is a type-level encoding of Booleans": this suggests that e contains
> just one bit of information; but the text says, e keeps track of which
> operations (writes, reads, freezes) are allowed to occur: this seems to
> require more than one bit of information. Could you clarify what are the
> possible values of e? -- OK, I see in the following paragraph that e remains
> abstract (a type variable) and can be qualified with HasPut/HasGet/etc.
> constraints. Thus, e really encodes a vector of Booleans whose width is
> not predetermined. Is the type variable e ever actually instantiated with
> a concrete type?

> p.49, although the HasPut/HasGet/etc. idea allows keeping track of which
> effects a computation *may* have, it is not clear whether it allows
> requiring a computation to *not* have a certain effect. E.g., can I
> write a function f whose argument *must* be a deterministic computation?
> (i.e., it must *not* be quasi-deterministic)
> -- I see (later on) that there is a distinction between runPar and
> runParQuasiDet, so I assume the former cannot be applied to a
> quasi-deterministic computation. But I cannot find where the types
> of these two functions are presented/explained.

> p.49, More generally, why is important/useful to statically keep track of
> effects like this?
> -- I am guessing that runPar and runParThenFreeze have a pure result type
> whereas runParQuasiDet has an IO result type. Assigning a more precise
> type to runPar and runParThenFreeze would be a motivation for statically
> keeping track of the HasFreeze effect. What about HasPut/HasGet?

> Section 4.3.1, you are using both "a Par monad" where "Par" is used as an
> adjective. But there is also "the Par monad" where "Par" is used as a noun. If
> I understand correctly, in the first sense "Par" means "ParMonad" and in the
> second sense "Par" means "Par". Perhaps the English should be clarified. Or,
> maybe the point is, there is no such thing as "the Par monad". That's what I
> understand at the end of 4.3.1. But if that is the case, what was "Par" in
> sections 4.2.1 to 4.2.5?

> 4.3.1 is entitled "Monad transformers", but it seems to be mainly about
> "Par-monads", not about transformers.

> The code on p.56 remains rather mysterious. Although I kind-of-understand the
> big picture, I am not sure which functions are standard library functions and
> which are defined by LVish (set, reify, pickLetter). I also don't know what
> the types of these functions are (set, reify, pickLetter, runParVecT,
> forkSTSplit). In fact, section 4.3.4 is supposed to present ParST, but it
> is presented only via an example; the reader has no idea what the API is.
> Would it be possible to show the API (or a fragment of it) in a figure?
> Last, when you write that ParST is "safe", is this is a fully
> static guarantee, or are some errors detected only at runtime?

> Section 4.4.2: based on my reading of section 4.2.2, I would expect the type
> LVar to be parameterized by a "session" s. Or is it a phantom parameter that
> can be added after the fact, when the final API is built?

> p.58, "the (mutable) data structure representing the lattice": do you mean
> that a value of type a represents a lattice *element*?

> p.59, who provides the type SchedQ? Is it defined as part of LVish, or defined
> by some library on top of which LVish builds? Same question about the operations
> "sched" and "exec" and "pushWork" and "newDedupCheck", etc. in Fig. 11.

> p.59, what is the type "Par lvl a" in the declaration of "mkPar"? I don't think
> we have seen a definition of "Par" yet. There was a type "Par" in section 4.2.5,
> but it had three arguments (and I don't know where it was defined either). I am
> beginning to be quite thoroughly confused at this point. I don't know how "mkPar"
> is defined (am I supposed to guess?) and I don't know what the parameter "lvl"
> means. (Why is it unconstrained in the result type of "mkPar"?)

> p.61, the text suggests that "idemp" (the fact that all writes are idempotent)
> is a property of a *computation*. So, why is "idemp" defined in a module
> called "Sched", which suggests that being idempotent is a property of a
> *scheduler*?  Does this mean that every computation somehow has its own
> tailor-made scheduler? There seems to be some hackery ("the type-level
> information [...] is reified [...]") that is not really explained.

> p.60 and 61, I could use clearer explanations about getLV. I think I get the
> big picture, and I think the code is clear, but the text isn't. I have
> difficulty relating the text and the code.

> p.61, the caption of Figure 12 isn't very clear. The fonts are inconsistent
> and the second sentence may be missing some words.

> p.62, again, I think I get the big picture, but I am not sure about some
> details. In the sentence "we add a marked bit to each CPU scheduler state",
> what is a "CPU scheduler state"? Is it "q", which previously was referred
> to as "a CPU-local scheduler queue"? And what does "CPU" mean? Does it refer
> to a physical core (so there is one scheduler state per physical core on the
> machine?), or is it some abstract notion of CPU (so there can be any number
> of "scheduler states" in existence?). Last question: the text suggests that
> "awaitClear" waits until it observes a clear mark bit on *every* CPU. Yet
> it takes a single "q" as an argument. Could you clarify?

> p.63, I am again confused as to the meaning of "Par": there is an occurrence
> of "Par e s HandlerPool" followed, a couple lines below, with an occurrence
> of "Par lvl ()". Are there two distinct types called "Par"? What are they?

> p.63, it may not be obvious to a reader who is not familiar with Haskell
> that "cnt" is the value of the field "numCallbacks" and that "bag" is the
> value of the field "blocked".

> p.63, the code of "quiesce" is shown, but the code of "addHandler" is omitted.
> As a result, the reader has an incomplete view; e.g., we see where "poll cnt"
> is called, but do we do not see where "cnt" is incremented and decremented.
> Similarly, we see where threads are added to the bag "blocked" and removed
> from the bag, but we do not see where this bag is actually used (and I am not
> sure that I can guess). I would suggest showing and explaining "addHandler"
> too.

> p.64, "a case study that fits this mold": what do you mean? does it fit (1),
> or (2), or both?

> p.66, maybe it would be useful to remind the reader that "forEach" was defined
> (in the case of sets) much earlier on p.30.

> p.66, "the programmer can largely ignore the temporal behavior": this holds as
> long as one reasons about the functional correctness of the program, I
> suppose. When one wishes to reason about its performance and diagnose
> performance problems, I imagine that a mental picture of the handler graph
> could be useful?

> p.66, "Finally, there is an optimization benefit to using handlers". I assume
> this idea is what was referred to as "partial deforestation" on p.65. However,
> it is not clear to me that this approach allocates fewer intermediate data
> structures than a naive approach based on flattening one layer at a time.
> Indeed, the nested calls to forEach cause a graph of listeners to be built in
> memory. What is the size of this graph? Isn't it just as large as the
> intermediate data structure that would be built in the naive approach?

> p.66, "copying them repeatedly and discarding them without modification".
> It is not clear which parts of the code this sentence refers to. Maybe it
> refers to parts of the code that have not been shown, such as the function
> "next"? This should be clarified (and the relevant bits of code should be
> shown).

> p.66, "Like the purely functional program from which it was ported, [...]".
> This is difficult to follow. The original purely functional program is not
> shown. I have difficulty guessing what it was and why it would use a copy
> operation (in a purely functional program, one doesn't need to ever copy a
> data structure, right?). Or, did the original program represent stores as
> mutable data structures? I am confused. For the same reason, I don't
> understand "the overhead of [...] copying in an idiomatic pure functional
> program".

> p.67, "for a total of up to 202x". This figure comes out of the blue and
> makes no sense if you don't state how many CPUs were used. (I understand
> that this information comes further on.)

> p.67, "a pure Data.Set wrapped in a mutable container". I suppose this
> implies that access is mediated by a lock?

> p.67, "it proved difficult to generate example inputs [...] that took
> long enough [...]". This is a bit surprising, as: 1- k-CFA is supposed
> to be costly, especially as k grows; and 2- if k-CFA is not costly,
> why did you choose it as a benchmark in the first place?
> Looking at Earl et al.'s paper reveals that "blur" is a tiny program
> of 40 AST nodes. How about applying k-CFA to a real code base?

> p.67, "was specifically designed [...] sharing approach". Could you explain
> what "[your] sharing approach" means in this sentence, and why this example
> would "negate" its benefits?

> In Figure 14, it would be good to provide the sequential baseline (i.e., what
> is the performance of the original purely functional program?). Similarly,
> concerning PhyBin, how does sequential PhyBin compare against parallel PhyBin
> run on one core?

> p.69, "and is about 2-3x times as fast as PhyBin". I assume this is a
> comparison between two sequential programs?

> Footnote 40. In fact, in a purely functional language, the whole heap can
> be thought of as a huge primitive LVar. Come to think of it, this raises
> the question: is there a lattice-generic analogue of garbage collection
> in an LVar? e.g. in a Map LVar, can one somehow remove map entries that
> provably will never be read and will never trigger a handler? The FlowPools
> paper seems to discuss something along these lines.

> TYPOS

-->

> p.4, "an specific"

> p.9, "Tesler & Enea, 1968 (Spring)". Maybe (Spring) is unneeded :-)

> p.30, should addInPool be addHandlerInPool? or addHandler? confused.

> p.30, "Early quiescence no risk"

> p.31, "the [...] pattern I described above": who is "I"?

> p.34, "in lambda_LVish, the put operations took two arguments": do you
> mean lambda_LVar?

> p.48, "both forks must compete" -> "complete"?

> p.49, "Section ??"

> p.51, "we are [...] calling getKey on the Book key, as before": are
> we? the code does not seem to contain a call to getKey.

> p.56 and 57, should "ellipses" be "..."?
> should "DOT" be "."?

All of these are fixed -- thank you!

> p.66, "handler events" : do you mean "event handlers"?

By "handler events", we meant "events that occur because of executing
a handler's callback".  We've changed it to simply "events".

> p.69, "psuedocode"

> p.76, "a more manageable" : missing word?

Fixed; thanks!

> Referee: 2

<--

> Comments to the Author
> The paper proposes a useful generalization of the write-once variables
> used for synchronization and communications in some models of
> deterministic parallel programming. It observes that instead of a
> single empty-full transition, a communication variable can be
> incrementally updated multiple times, as long as all the updates are
> monotonically increasing in information content. The paper develops a
> formal model of such a programming paradigm, and shows that it
> satisfies a desirable determinism property. It also shows how the
> basic model can be further generalized, allowing for some more
> convenient programming idioms, at the cost of a limited weakening of
> the determinism guarantees.

> While the overall development seems sound and well motivated, the
> paper is arguably longer than its need to be, partly due to a
> substantial amount of repetition. I also have some reservations about
> specific aspects of the presentation and technical details, as
> detailed below. I believe these should be addressed somehow before I
> can unreservedly recommend acceptance of the paper.

> --

-->

> 1\. In my opinion, the key definitions are considerably uglified by the
> insistence on the ordered set D being a "lattice" (actually, a bounded
> join-semilattice), and specifically the requirement that any two
> elements of D must have a designated least upper bound, possibly Top.
> This top element is treated completely differently from all the
> regular elements of D in the operational semantics, in that it causes
> execution to abort immediately; and likewise, it plays a special role
> in the definition of well-formed threshold sets. In several other key
> definitions there is also a separate clause for "Top" and one for all
> other all other elements, cluttering the the development

> Further, when extending the lattice ordering to stores (= finite maps
> from locations to lattice values), again a strong distinction is made
> between "proper" stores, in which all locations are mapped to non-top
> elements, and a special "error store" Top_S, which itself cannot
> evolve any further.

> I think this would all be handled more elegantly by stipulating that
> the error element is _not_ considered a part of the lattice D, but
> that the "lub" operation is instead in general a partial function, and
> is explicitly undefined for incompatible pairs of operands. The
> requirement would then be that lub(d1,d2) is defined for any pair of
> elements d1 and d2 that have an upper bound at all. Likewise, all
> stores would now be "proper", but the lub operation on stores would
> again be partial. This would also eliminate the need for
> configurations of the form <top_S;e>, and in particular avoid the
> dubious overloading of _syntactic_ equality on configuration, i.e.,
> the stipulation that <top_S;e> = <top_S;e'> = error for all
> expressions e and e'.

> Decoupling greatest elements from errors would also rectify an anomaly
> for lattices in which all elements are actually compatible, such as
> the set of subsets of a finite set (as used, e.g., in many program
> analyses, including the k-CFA in Section 4.5), in which the lub
> (union) of two subsets may be the whole set, without that indicating
> some error condition or inconsistency. In the current model, such a
> lattice must be artificially extended with an extra super-top element,
> strictly above all the others, to avoid the natural top element being
> given an inappropriate significance.

> Still, whether Top should be considered an element of D (just to make
> lub a total function) is arguably just a presentiational preference.
> But there is a more fundamental underlying issue: are lubs important
> to the model at all? As the paper points out in 3.2.3, the "put"
> operation of lambda-LVar can be generalized to a whole family of (not
> necessarily idempotent) update functions in lambda-LVish - crucially
> _without_ weakening the determinism guarantees of LVar (unlike some of
> the other extensions of lambda-LVish, such as freezing). That is,
> taking put_i(d) = lub(d,d_i) is a particularly simple way of
> constructing a set of update functions satisfying the conditions of
> Def. 3.3, but clearly far from the only one.

> The two conditions both seem quite natural for the purpose of ensuring
> determinism: an update operation should be inflationary, so as not to
> retroactively invalidate a previous threshold-read transition, and the
> order of parallel updates should not matter. On the other hand, it
> seems unnecessarily strong to require that _any_ two elements of D
> (and, by, extension, any two stores), even if they did not arise from
> two finite sequences of updates from the same starting point, must
> necessarily have a lub in the order.

The reviewer makes two key points here: first, that the way we have to
handle the top element clutters things up a little, and second, that
lub updates are something of a red herring.  These are both good
points.

> What I would therefore concretely suggest is to first move the
> generalization to non-put updates (as already described once in 2.6)
> into the section on lambda-LVar, and immediately characterize the
> lub-based "put"-operations as just one particular way of constructing
> a family of update functions.  Indeed, footnote 19 explicitly on p.44
> asserts that this can be done, so presumably the authors have already
> verified that all the relevant lemmas and theorems go through, even if
> the proofs (which are not even sketched, anyway) get more complicated.

> This would eliminates a fair bit of redundancy and repetition, and
> also make a cleaner distinction between the fully deterministic
> lambda-LVar and the only quasi-deterministic lambda-LVish.

> (If it is desirable for consistency with the authors' previous work to
> still refer to the put-based language as lambda-LVar, the variant with
> generalized updates could be called lambda-LVar+, or similar.)

We've adopted the reviewer's suggestion of having arbitrary updates be
part of lambdaLVar, for the reasons reviewer 2 gives as well as the
reasons I gave above in response to reviewer 1.  (As far as
consistency with previous work, that ship has already sailed; what we
presented in the first submitted version was already very different
from the version of lambdaLVar that appeared in our FHPC '13 paper.)

> Second, with the lubs on D no longer directly present in the
> operational semantics, it should be possible to cleanly reconsider
> where exactly they _are_ used (other than trivially, in conditions of
> the form "lub(d1,d2) = Top") in the determinism proof, and thus
> whether D really needs to be a (semi)lattice at all. Note that the
> generalization of Lemma 2.5 to Lemma 3.7 also obviates the need for a
> lub operation on stores. And as near as I can tell, even in the full
> LVish library, a datatype implementer need not implement a function
> computing lubs of arbitrary elements, suggesting that it's not really
> a requirement.

> But if there isn't a solid reason for why the existence of all D-lubs
> (of elements that have common upper bounds at all) is essential, the
> whole reference to "lattice-based" (already technically imprecise,
> since no meets are required) in the title is misleading, and would be
> more accurately named "poset-based" or similar. This really needs to
> be resolved.

We think that requiring a partial function that takes a pair of
states, instead of requiring a join-semilattice with lubs, would mean
a fundamental change to the flavor and focus of the work.  In the
distributed systems community, the work on conflict-free replicated
data types that we cite in section 5 made the observation that the
"conflict resolution functions" that Dynamo called for could in fact
often be seen as least upper bounds in a join-semilattice, and explot
join-semilattice properties to get distributed consistency.
Similarly, we want to exploit join-semilattice properties to get
determinism.  Indeed, a join-semilattice isn't always the most natural
fit for the data.  But, excitingly, it often is, and it's in those
situations where LVars really shine.

We see arbitrary updates as an interesting generalization that allows
us to conveniently handle more kinds of data.  If we hadn't added that
generalization, and the model still lacked arbitrary update operations
(as was the case in our POPL '14 paper), maybe the reviewer would not
have minded our focus on lubs.  Rather than engendering disappointment
that we haven't taken the generalization even further, we hope that
this paper will engender enthusiasm that such a generalization to
LVars (which this paper is presenting for the first time) is possible.

Furthermore, in the common case where all updates *are* least upper
bounds, we enjoy idempotence of updates -- which makes possible some
clever implementation tricks, as discussed in our POPL paper.  In any
case, the name "LVars" is pretty entrenched.

All that said, we are happy to take some of the emphasis off lubs, and
we hope to accomplish that by moving the arbitrary update feature to
lambdaLVar.

> --

> 2\. It is rather unfortunate that the determinism result (Theorem 2.1)
> specifically does not address consistent termination (i.e., that if
> one run terminates with a non-error result, another run cannot
> diverge), but that this is only left as a conjecture. I think that,
> especially for an archival publication, the conjecture ought to be
> resolved (presumably positively), or there should at least be given a
> clear argument for why such a resolution would be a particularly hard
> undertaking, beyond the reasonable scope of the paper.

> Note that the determinism theorem already establishes a very tight
> correspondence between different runs of a program: any two
> successfully terminated computations from the same starting
> configuration must yield stores that differ only by a permutation on
> the locations, and so in particular, the two runs must have performed
> equally many allocations.  Since any program can be peppered with
> additional dummy allocations, this effectively means that all
> successful runs of a program perform the same number of basic
> operations, only perhaps in a different order. Given this, it seems
> particularly unlikely that one could construct a scenario in which one
> run terminates successfully after n transition steps, while another
> performs even n+1, let alone infinitely many, such steps.

I think the reviewer has a good point.  I'll need to think a little
more about this termination thing.  Maybe Neel has thoughts on this?

> --

> 3. The strong correspondence between all runs of an error-free
> lambda-LVars program also suggests that the conclusion on p.75 that
> the LVars model subsumes or accommodates the identified major
> paradigms for deterministic parallel programming may be a little
> premature. Specifically, the proposed execution model has as a
> fundamental assumption that all threads must run to completion, or at
> least quiescence, before the whole program can successfully terminate.
> That is a rather strong requirement, and clearly not needed for
> guaranteeing deterministic parallelism.

> For a simple example, consider the "parallel or" function on two
> bool-producing subcomputations: it returns true when either
> subcomputation returns true (even if the other one hasn't terminated
> yet, or ever will), and false when both return false. This function is
> evidently effectively computable and deterministic, but doesn't seem
> to be expressible in lambda-LVar, or even lambda-LVish. What appears
> missing is a way to allow a whole parallel subcomputation to be
> terminated (or at least quiesced) early, when its ultimate result, if
> any, provably cannot influence the final result of the whole
> computation

> (An analogous, and probably more practically important, situation
> occurs in, e.g., a parallel branch-and-bound search, where exploration
> of any partial solutions that cannot possibly be extended to a full
> solution better than one already identified, may be abandoned
> immediately, and the corresponding processor reassigned to exploring
> solutions with better prospects. But for the purpose of discussion,
> the basic parallel-or example is much simpler to analyze.)

> I would be really useful to have an account of how, in the authors'
> estimation, parallel-or-like operations fit into the "deterministic
> parallel" landscape, and whether/how they could be accommodated in the
> LVar/LVish model, or a plausible extension thereof.

This is a great question.  We've added a section in 2.6 discussing how
to express short-circuiting parallel "and" in our model,.  To do so,
we need to slightly generalize the notion of threshold sets.  Along
with that, we've included another section that discusses generalizing
threshold sets even further, to "threshold functions" -- this section
has something of the flavor of the partial functions that the reviewer
mentions above.  This material all appeared in Kuper's dissertation
but was left out of our original submission as a way to save space.
Now that we are cutting some implementation-focused material and some
of the repetitive material in chapter 3, some space has been freed up
to include it.

> --

> 4. Also in relation to the comparison with other models, the assertion
> that the LVars model can subsume classic dataflow paradigms, such as
> Kahn process networks, is also essentially unsubstantiated by anything
> in the development. If is indeed a fairly immediate observation that
> channel histories are monotonically increasing under the prefix
> ordering, but that doesn't automatically mean that a dataflow network
> can be usefully encoded in the proposed LVar/LVish computation
> model. In particular:

> * How are channel read/write operations concretely realized in terms
>   of a suitable family of update and threshold-read functions? In
>   particular, the natural update operation of appending an element to
>   a channel history is indeed inflationary, but any two such updates
>   evidently do not commute. (Indeed, with only a single writer per
>   channel, as in Kahn networks, they don't need to commute, but that
>   invariant may not be inherently ensured by an embedding in
>   LVar/LVish.)

> * In the lattice of channel histories (or simply streams), an LVar can
>   in general be properly increased an unbounded number of times. But
>   that means that even in a simple producer-consumer network, there
>   needs to be some additional mechanism, beyond LVars, for ensuring
>   that an unconstrained producer (say, one that just emits the stream
>   of natural numbers) does not attempt to produce infinitely many
>   elements, before giving the consumer a chance to process them. One
>   needs to either assume a fair scheduler (a fairly strong
>   requirement), or add explicit flow control to quench an overactive
>   producer. While the latter can probably be implemented, even for the
>   more challenging case of multiple-reader channels, reasoning about
>   the resulting system would presumably not be nearly as clean and
>   simple as about the original process network.

> These issues should be convincingly addressed, or the assertion that
> LVar/LVish subsume dataflow models should be removed or rephrased.

This echoes the point that reviewer 1 made that our claim that we
subsume KPNs is too strong, and we agree.  We've toned down that
claim.

> --

> 5. Finally, I'm quite concerned by the fact that in both lambda-LVar
> and lambda-LVish, the grammar stipulates that lattice elements,
> threshold sets, etc. can only be given as literals in the program
> text, but but not as the result of a non-trivial computation. For
> example, in lambda-LVar, since the number of distinct d-values in a
> program program text is necessarily finite, and there is no way to
> construct new ones at runtime, it is clear that any LVar can only have
> its value properly increased a finite number of times during any
> program run (finite or infinite).  Likewise, since threshold sets must
> occur as literals in the program text (though it's a little unclear
> how one would even write down an infinite set in this way), they
> cannot be made to depend on previously computed values. And in
> lambda-LVish, while the location to be updated may be result of a
> computation may be dynamically computed, the parameter i to the
> put-function must still be statically fixed in the program text.

> Most problematically, however, there isn't apparently any way to do a
> conditional branch based on a D-value (e.g., one returned by a
> get-operation). One might therefore reasonably worry that the
> determinism property, or at least the simplicity of its proof, is
> ultimately due to one of these restrictions. Since almost all the
> non-trivial examples in Section 4 do rely on run-time branching, or
> other computations on D-elements it seems dubious to exclude them
> entirely from the model language.

> A simple, general solution to this problem would be to not have
> multiple distinct _syntactic_ categories for the various semantic
> objects such as threshold sets (which don't in general even
> necessarily have finite textual representations). Rather, there should
> be just a single domain V of atomic _semantic_ values (specifically
> excluding lambda-abstractions \x.e, and locations l), together with an
> unspecified collection of primitive-function symbols working over
> values of this domain. These function symbols would denote suitable
> mathematical partial functions on V, and would typically also include
> ordinary arithmetic and numeric comparison operators, as found in many
> of the examples. (Also, by choosing the set of threshold-set-producing
> functions carefully, it could be ensured that only sets satisfying the
> incomp(.) predicate could be constructed in the first place, obviating
> the need for a nominal runtime check in the E-Get rule.)

> In the operational semantics, applications of a function symbols to a
> tuple of fully evaluated arguments (literals) would then, in a single
> transition step, be replaced by the result of the function application
> (or 'error' if outside the domain of the function). And crucially,
> addition to the existing LVar/LVish primitives (get, put, new, etc.),
> the expression language could now include a standard if-then-else form
> (where the test-expression would be expected to evaluate to a boolean
> value, or perhaps just treat () as false and all other values as true.

> If things are set up properly, the exact collection of primitive
> functions shouldn't actually matter for the determinism proof. But
> note that it does matter what exactly gets included in V, as opposed
> to the "syntactic" values. For instance, if locations (represented as
> numeric addresses) are included among the semantic values, and one of
> the primitive function symbols denotes the arithmetic less-than
> comparison operation, the order of allocation of two LVars would
> actually matter, making the semantics nondeterministic, even if "new"
> is nailed down to always return exactly the lowest address not yet in
> the domain of the store.

> If it should turn out that the above generalization is _not_ possible,
> but that, e.g., all threshold sets do need to be given as literals in
> the program text, that would not necessarily invalidate the basic
> model, but it would make it much clearer exactly what it assumes, and
> what might be the issues in scaling it up.

I'm still thinking about how to summarize and respond to this point.

<!--

> -- Comparatively minor issues and misprints:

-->

> - p.14, l.6. What happens if a thread explicitly "put"s a Top value?
> If Top is by definition a proper element of D, this could technically
> happen.

> - p.14, 2.3.2, l.2. "countable" -> "countably infinite".

> - p.14. It is rather unfortunate to use the notations "Top_S" and
> "\sqcup_S", where the subscript S looks just like a metavariable
> ranging over stores. Please use a different font (e.g., non-italic)
> for any "S" that is part of an operator name, as opposed to a
> parameter.

> - p.14, footnote 6. Avoid "I", especially in a multiple-authored
> paper.

> - p.15, Def. 2.4. Does equality on stores need to be explicitly
> defined at all? It's just the normal equality relation on the
> set "(Loc -> D-{Top}) U {Top_S}".

All of the above are fixed -- thanks!

> - p.15, 2.3.3, l.13. Can a threshold set contain Top? Nothing seems to
> explicitly prohibit it, but without such a a restriction, the
> uniqueness argument technically fails: consider the flat lattice of
> integers in Fig.2(b), with T={3,Top} (which evidently satisfies
> incomp(T)), and let d1=Top.  Then a threshold read could legitimately
> return either d2=Top or d2'=3.

This couldn't happen, because there can't be a value of Top in S
(because any write that would go to Top would instead step to error).
Although we don't explicitly prohibit putting a Top element in a
threshold set, there's no point in ever doing it, because the LVar
could never reach Top and unblock at that element.

> - p.15, l.-9. The reference to Figure 2(b) should presumably be 2(c),
> since the example talks about pairs of integers.

Fixed; thanks!

> - p.15, l.-3. The forward reference to 2.6 says that it will discuss a
> generalization of threshold sets, but the actual 2.6 doesn't talk
> about threshold sets at all, only about generalized updates.

Thanks; that forward reference makes sense now that we've added more
material to 2.6.

> - p.16, 2.4. Both lambda-LVar and lambda-LVish are CBV languages, but
> the LVish library is ultimately embedded in Haskell, a CBN language.
> At least a few words of explanation are needed.

We've added a footnote to the part of section 2.4 that discusses the
CBV-ness of lambdaLVar to explain that we accomplish this with
strictness annotations in LVish.

> - p.20, l.-11. It's not really Def. 2.6 of permutations that's being
> "lifted" to expressions, stores, and configurations, but rather the
> notion of _applying_ the permutation, i.e., the notation pi(e), pi(S),
> and pi(sigma). I think it would be useful to actually write out the
> relevant definitions.

Hmm.  They're in my dissertation, but I specifically left them out
here, because I thought they were pretty boring!  But I'll revisit
this. [TODO]

> - p.21, l.3. Don't overload the word "value" (which already has a
> precise definition in the grammar) to also mean "a configuration that
> cannot step".  For the latter, use "result", "final outcome",
> "terminal configuration" or similar.

Thanks; we've changed this to "result".

<!--

> In fact, a bit more precision may be in order. There are conceptually
> 5 observably different possible outcomes of a lambda-LVar computation:

> 1. a value: a successful termination
> 2. a runtime error: conflicting puts on the same LVar.
> 3a. a deadlock: some "get"-redexes remain, but all are blocked
> 3b. a "type error": e.g., applying a non-lambda value to an argument,
>     or attempting a put on a non-location value.
> 3c. a "threshold error": a get operation with an ill-formed threshold set
> 4. divergence: reduction goes on for ever

> The semantics does not distinguish formally between 3a, 3b, and 3c,
> which I'm not sure is the best choice (arguably 3b and/or 3c belong in
> category 2), but presumably a simple static analysis can eliminate 3b,
> and 3c can be prevented by only allowing datatype implementors (as
> formalized, e.g. by the primitive functions suggested above) to
> construct threshold sets.

-->

> - p.23, Lemma 2.5. The paper gives absolutely no indication of where
> and how the Independence property is used in the proof of
> determinism. It is formulated in terms of "lub-ing on" some other
> store to a transition, but where does one actually need to do that in
> the formal development? It is fine to omit detailed proofs of the
> various lemmas and theorems, but for each one, please at least sketch
> in a few words how it was proven (e.g., by induction on one of the
> derivations), and in particular which previous lemmas the proof relies
> on.

> Footnote 13 is not helpful in this regard, since 5.6 just talks more
> about Hoare-like notation and doesn't really talk about LVars at all.

The reviewer has a good point.  These proof sketches appear in my
dissertation but not in this paper.  I'll add them.  (For this case in
particular, it's the proof sketch for Strong Local Confluence that
mentions Independence.)  [TODO]

> - p.25, Theorem 2.1 (Determinism). No proof of this theorem (or most
> of its key lemmas) is given, but evidently it does not hold as
> stated. Let D be the vertical-naturals lattice from Fig.2(a), and let
> l0 and l1 be distinct locations. Then consider the following
> configuration:

>   sigma = <[]; (\x.put l0 3) new>

> Now, if in the E-New rule, l is chosen as l0 (which is not in the
> domain of the empty store), then sigma transitions in three steps to
> sigma' = <[l0 |-> 3]; ()>, which evidently cannot reduce any
> further. On the other hand, if l is chosen as l1, sigma transitions in
> two steps to the stuck configuration sigma'' = <[l1 |-> bot]; put l0 3>.
> But clearly there is no permutation pi on locations that would
> make sigma' = pi(sigma'').

> What appears missing is the implicit invariant that, in a "proper"
> configuration <S;e>, all locations occurring in e are also in the
> domain of S. It would actually be useful to know which of the lemmas
> need this extra property, and for which ones it doesn't matter.

That's right; if a location hasn't been allocated yet, then we can't
refer to it in the program.  In fact, the only way to get a location
name is by calling `new` and saving the result in a variable.  I
thought we already noted this explicitly, but it seems that we don't,
and we should! [TODO]

> - p.25, l-4. "Figure 2(c)" should presumably be "Figure 2(a)" (the
> lattice of vertical numbers, not of number pairs).

> - p.29, Ex. 3.5. Add parentheses around "x+1" for disambiguation.

These are both fixed.

> - p.31, l.-12. This is another instance where the lattice of sets,
> with set union as lub, does not quite give the correct results,
> because the top element of the lattice does not represent an
> inconsistency.

We're not sure what the reviewer means here by "does not quite give
the correct results".

> - p.31, l.-9. "I" -> "we".

> - p.32, 3.2.1. This is virtually identical to the previous definition.
> Is it really necessary to repeat it?

These are fixed; thanks!

<!--

> - p.33, Lemma 3.2. Formally, properties 2 and 3 could be joined into
> simply "every finite subset of D_p has a lub" (since Bot is precisely
> the lub of the empty set). I'm not sure if this is easier to work
> with, though.

-->

> - p.34, l.-15. "lambda-LVish" -> "lambda-LVar".

Fixed; thanks.

> - p.34, l.-4: "Every set of updates implicitly contains identity".
> While identity can obviously be added to any family U of updates,
> that's not quite the same as requiring that it already be there.
> Indeed it might be useful in some contexts that it is not, e.g., so
> that every update actually has an effect, which, in a lattice of
> finite height, could be the core of a termination argument.  Also, it
> formally prevents U from being a (nontrivial) singleton set; for
> example, for the vertical natural numbers, U cannot be just the
> increment function.

> If U is required to contain identity, it could also make sense to
> require it to be closed under composition, i.e., if u_i and u_j belong
> to U, then so does u_i o u_j (= u_j o u_i). (Note that it does not
> seem necessary to require every element of U to actually be directly
> denotable as a single put_i function, so this is meant more as a
> semantic completeness property.)

That's a good point about termination arguments.  We required that the
identity function be part of every set U because it made it easier to
write the definition of the store update operation U_S.  But, we
should revisit that anyway. [TODO]

> - p.36, Fig.8. In the grammar, the syntactic form "freeze l after Q
> with \x.e0, {e..}, H" is presumably not meant to be writable directly
> by a programmer, but only arises by the transition E-Freeze-Init, and
> is incrementally updated by E-Spawn-Handler.  However, formally, the
> quasi-determinism result (Thm. 3.1) is stated to hold for all
> configurations sigma, including ones with completely arbitrarily
> chosen {e,...} and H, with no relations to Q or e0.  Just to confirm:
> is this indeed the case, or are there any further restrictions on
> "proper" configurations needed, for example that H is always a subset
> of Q?

That's right; that syntactic form only comes about via E-Freeze-Init.
Section 3.2.6 explains more about its semantics; there, we say, "The
set H (a subset of the lattice D) represents those values in Q for
which callbacks have already been launched".  In other worse, yes, H
is a subset of Q.

> Also, the notation used for the set of running handlers strongly
> suggests that this set could be infinite - like, e.g. threshold sets
> P, and unlike handled sets H. Presumably this is not intended (what
> would it even mean to have literally infinitely many handlers running
> at once?), so the set should be written as {e1,...,en} everywhere (and
> in the last line, its elements should be separated by commas, not
> juxtaposition, since this overlaps with the notation for function
> applications).

Yes, the reviewer is correct that the notation should suggest that Q
is a finite set.  We should fix this. [TODO]

The missing commas were added in an earlier edit.

> - p.36, l,-2. The references should be to Figures 9 and 10, not 4 and 5.

Oops!  Fixed.

> - p.37. It appears that "freeze e" can be seen as just syntactic sugar
> for "freeze e after {} with \x.x". Is there any need to treat it
> explicitly in the grammar or operational semantics?

That's a great point!  I think the reviewer is right; we don't have to
have E-Freeze-Simple or `freeze e` explicitly. [TODO]

> - p.40, l.7. "the lub of those values" does not refer to anything
> identifiable in the preceding text. It should presumably "the LVar's
> value after the last update" or similar.

Good catch; we've changed this to "the contents of the LVar after all
updates have been applied."

> - p.40, 3.3. Unlike for lambda-LVar, a consistent-termination property
> for lambda-LVish is not even conjectured. However, given the same
> tight correspondence between successful final results (in particular,
> the same number of allocations), presumably the proof would not be
> significantly complicated by freezing and/or generalized updates.

That's correct -- we don't bring up consistent termination again,
because there is nothing new to say in the lambdaLVish setting.

> - p.40-41. The formal parts of sections 3.3.1-3.3.4 seem to exact
> copies of the corresponding definitions and lemmas from the LVar
> language, only now meaning something slightly different because the
> underlying definitions have changed. This seems to be a waste of
> space. (In general, there's a lot of unnecessary verbatim repetition
> between lambda-LVar and lambda-LVish, such as Fig.10 on p.38, but here
> it's particularly egregious.)

It's true, this part is really repetitive and we can probably
eliminate a lot of that.  In fact, we can probably replace the
repeated definitions and lemmas with a couple sentences, phrased
something like how the reviewer has: these definitions are the same as
before, but meaning something slightly different. [TODO]

> - p.43, l.2. If U is a set of update functions u_i on D, and U_p is a
> set of update functions u_{p_i} on P (= D x {true,false}), one would
> logically expect U_S to be a set of update functions on stores (finite
> mappings from locations to P), but it's not. Rather, it's just one
> such mapping. Please use a more consistent naming convention.  (And
> it's particularly confusing that in "u_i", i is a variable/parameter,
> with i ranging over some set I, while in "U_S", S is part of the name.

As the reviewer suggested doing earlier, we updated all the notation
that had S as part of the name to use a non-italic S to make it more
clear that it's not a parameter; U_S is one of those.  That should
help make this more clear.

> More problematically, however, the whole definition of a "store update
> operation" is formulated in such a way that the intended mathematical
> meaning is hard to discern. In particular:

> * What, exactly, can the choice between freezing and updating depend
>   on? For example, supposing that locations are represented as just
>   addresses, and the lattice D is the vertical numbers from Fig.2(a),
>   would it be a valid update operation to increment all the
>   odd-numbered locations and freeze the even-numbered ones? Note that
>   such an update function would most likely upset the general property
>   of invariance under permutations.

> * In the update case, what can the choice of update function u_(p_i)
>   in U_p depend on? Presumably the index p_i may at least depend on l
>   (otherwise, all locations would have to be updated in the same way),
>   but one would assume that it cannot actually depend on the concrete
>   representation of the location (e.g., increment each cell by its own
>   address), nor on the rest of S itself. This should be uniformly
>   resolved by replacing the textual "for each ... for some ..."
>   quantifiers by explicit functions.

> * The last clause should presumably end, "... and some frz in
>   {true,false}", since frz is not quantified elsewhere. But then, what
>   is the clause really requiring, i.e., how could the condition
>   possibly fail at all?

The reviewer makes a good point that the definition of store update
operation is mathematically sloppy.  All we are really trying to do
here is generalize `S \lub_S S''` to the case of non-lub writes.  If S
is a store, `U_S(S)` is a store that is at least as "big" as S, and
possibly bigger, because some of the elements may have had
non-identity state update operations U_p applied to them.  We can't
just say that `U_S(S)` is just S but with one state update operation
to everything in S, because `U_S(S)` may also contain new bindings.
I'm not sure what the cleanest way is to say this mathematically is,
but I'll think about it some more. [TODO]

> - p.43, Lemma 3.7. It's hard on the reader that the statement of the
> lemma refers to the notions of non-conflicting updates and
> freeze-safety, both of which are only defined later (in Def. 3.10 and
> 3.11).

Hm -- I actually thought it was easier on the reader this way, because
the definitions are quite technical and I thought it would be nice to
present the overall theorem first and then explain the fiddly bits
second.  But, I'm okay either way; we can change it. [TODO]

> - p.43, Def. 3.10. The last part, "U_S neither creates ..." should
> preferably be expressed in symbols, like in the textual explanation
> above the definition, rather than the other way around.

Fixed; thanks!

> - p.44, Def. 3.11. Likewise, express the requirement in symbols, in
> addition to (or instead of) textually.

Yeah, that's a good idea. [TODO]

> - p.48, 4.2.1, l.1, "one of our previous IVar programs". What does
> this refer to?

> - p.50, l.-3. "incrementable counters, as in the previous section".
> Which section is this, exactly?

> - p.51, 4.2.4, just below the code snippet: "... and then calling
> getKey on the Book key". This does not refer to anything identifiable
> in the code.

These are all fixed; thanks!

> - p.56, bottom. Too much context is missing to allow the reader to
> make much sense out of the code snippet. (And should "ellipses" in the
> last line have been "...")?

Indeed, the old code snippet was a bit nonsensical! This section's
been entirely rewritten with a different code example.

> - p.57, l.5. "(We will see shortly how this generalizes.)". This does
> not seem to refer to anything in the following text.

> - p.57, footnote 27. Should "DOT" and "ellipses" have been "." and "..."?

These are fixed; thanks!

<!--

> - p.71, section 5. It might be relevant to also mention Concurrent
> Constraint Programming [e.g., Saraswat et al., POPL'91], which
> likewise is based on an ask-tell model and monotonically increasing
> information (but in a general constraint domain, not just about a
> single cell value), and quiescence detection as a synchronization
> mechanism.

-->
