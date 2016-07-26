Thanks to both reviewers for their detailed reading of the paper and
suggestions for improving it.  We have already incorporated many of
the reviewers' suggestions.  Here we briefly outline the changes we
have made and plan to make.

## Reviewer 1

Reviewer 1 made three major suggestions:

>  1\. Serious thought given to the suggestion of merging lambda_LVar and
>     lambda_LVish into just one calculus, so as to reduce duplication and
>     shorten the paper. The new calculus should be as simple as lambda_LVar and
>     as powerful as lambda_LVish. Is it possible? I would like to think so, but
>     perhaps I am being naive.

>  2\. An explicit presentation of the LVish API, possibly at the cost of
>     consuming extra space;

>  3\. A fuller presentation and a better explanation of the LVish implementation.

We address each of these in order.

>  1\. Serious thought given to the suggestion of merging lambda_LVar and
>     lambda_LVish into just one calculus, so as to reduce duplication and
>     shorten the paper. The new calculus should be as simple as lambda_LVar and
>     as powerful as lambda_LVish. Is it possible? I would like to think so, but
>     perhaps I am being naive.

One calculus with the simplicity of lambdaLVar and the power of
lambdaLVish would be great if it were possible.  However, having given
it some serious thought, we do not think it is possible to "have our
cake and eat it too" here, because the main thing that complicates the
definition of lambdaLVish is the addition of handlers/quiescence,
which is also what makes lambdaLVish more powerful than lambdaLVar.

However, in the interest of reducing duplication and shortening the
ppaer, we agree with the reviewer that presentation-wise, it's not
necessary to go through all the steps of the (quasi-)determinism proof
twice, once for lambdaLVar and once for lambdaLVish (and reviewer 2
made a similar point).  We have therefore made section 3 significantly
less repetitive.

Furthermore, and most importantly, we've improved the presentation by
adding non-idempotent update operations (rather than just
least-upper-bound writes) to the original calculus, lambdaLVar.  This
makes lambdaLVar slightly more complicated, but in return, it means
that there is a smaller leap from lambdaLVar to lambdaLVish, and it
helps illustrate that the non-idempotent update operations in
lambdaLVish are *not* the source of its nondeterminism.  It also
de-emphasizes least-upper-bound writes in favor of arbitrary
inflationary-commutative writes (which include, but are not limited
to, least-upper-bound writes).

lambdaLVish now adds only two new features to the language:
handlers/quiescence, and freezing.  We think this makes sense
pedagogically, because these two features are especially useful in
combination.

The reviewer suggested that we could simplify lambdaLVish by viewing
freeze as just another update operation.  However, since unlike other
update operations, freeze returns a value (it performs a read as well
as a write), it is different enough from other update operations that
we feel it should be kept separate.  Moreover, it is the one feature
that introduces any form of nondeterminism.  Therefore, we think it
makes sense to not bundle it in with other udpates.

>  2\. An explicit presentation of the LVish API, possibly at the cost of
>     consuming extra space;

We plan to adopt this suggestion; section 4.2 of the paper will begin
with an explicit presentation of the essential LVish API operations
and types.

[TODO -- see Issue #6]

>  3\. A fuller presentation and a better explanation of the LVish implementation.

We completely agree with the reviewer that this section of the
originally submitted paper was hard to understand.  After putting some
thought into this, we have decided that the best approach is to leave
discussion of LVish internals out of this paper entirely.  The space
we save in doing so can be devoted to a better presentation of the
API.

The code that we presented in the original version of the paper
submitted in December is already out of date, and the implementation
is likely to continue to evolve. Nor does it do the reader any good to
simply repeat the now-very-outdated implementation section that
appeared in our POPL '14 paper. (The material in that paper about
leveraging idempotency is out of date, now that we allow
non-idempotent updates.)

The internals of LVish are more complicated than they used to be, and
still in flux.  We expect it to be the case for some time that the
best place to go to read up-to-date LVish scheduler code is the
open-source LVish repository, rather than any paper.

## Reviewer 2

Reviewer 2 made five major suggestions.  We will try to summarize each
of these suggestions briefly.

> 1\. *Rather than join-semilattices and lub updates, focus on posets
> and a more general form of update, starting from early in the
> paper.*

> 2\. *Give an answer to the question about consistent termination.*

> 3\. *Give an account of how we deal with early termination and parallel-or.*

> 4\. *Either substantiate or remove the claim that we subsume Kahn
> process networks.*

> 5\. *Have lattice elements and threshold sets belong to the same
> semantic category as ordinary program expressions, and make it
> possible to do, for example, conditional branching on them.*

We address each of these in order.

> 1\. *Rather than join-semilattices and lub updates, focus on posets
> and a more general form of update, starting from early in the
> paper.*

[LK: I'm now unconvinced about the below.  If lubs are generalized to
inflationary-commutative writes, why does it need to be a
join-semilattice at all?  It can just be a partially ordered set.  (It
still needs a partial order because we need the order to be able to
define "inflationary".)]

We have adopted the reviewer's suggestion of introducing
commutative-inflationary updates (which generalize lub updates) early
in the paper as part of lambdaLVar.

However, lub updates are an important enough special case that we
prefer to keep the join-semilattice data structure.  Removing it would
mean a fundamental change to the flavor and focus of the work, which
is about exploiting join-semilattice properties to get
(quasi-)determinism.  Although a join-semilattice is not always the
most natural fit for the data at hand.  But, excitingly, it often is,
and it's in those settings where LVars are especially useful.  We see
non-lub commutative-inflationary update operations as a useful
generalization that allows us to conveniently handle more kinds of
data structures, such as incrementable counters.

> 2\. *Give an answer to the question about consistent termination.*

[TODO -- see issue #7]

> 3\. *Give an account of how we deal with early termination and parallel-or.*

We are glad that the reviewer raises this point.  We've added a new
subsection to section 2 that discusses how to express a
short-circuiting parallel-and computation in our model.  (The idea for
parallel-or is essentially the same.)  Doing so requires a slight
generalization of threshold sets.  Along with that, we've included
another section that discusses generalizing threshold sets even
further, to "threshold functions".  (This material all appeared in the
first author's dissertation, but was left out of our original
submission as a way to save space.  Now that some space has been freed
up by cutting some implementation-focused material and some of the
more repetitive parts of the proof in chapter 3,  freed
up to include it.)

> 4\. *Either substantiate or remove the claim that we subsume Kahn process networks.*

We agree with the reviewer that this claim was too strong (and
reviewer 1 commented on this as well), and we've toned down the claim
in the couple of places where it appears in the paper.

[TODO -- see issue #5]

> 5\. *Have lattice elements and threshold sets belong to the same
> semantic category as ordinary program expressions, and make it
> possible to do, for example, conditional branching on them.*

To summarize the reviewer's complaints about the current design:

  * You can only write down lattice elements and threshold sets as
    literals in the program text, not as the result of a non-trivial
    computation;
  * moreover, you cannot do a conditional branch on the result of a
    get operation;
  * and the determinism of the system seems to hinge on these
    restrictions -- or one might worry that it does.

The reviewer is correct that lambdaLVar/lambdaLVish offer a very
restricted programming model.  We want to be clear about what exactly
these calculi are and are not meant to model.  lambdaLVar/lambdaLVish
are intended to demonstrate that it is possible to do
guaranteed-deterministic shared-memory parallel programming, by
restricting what kinds of updates can be made by a write and what
kinds of observations can be made by a read.  The calculi can be
thought of as modeling the kinds of operations that can take place
inside a Par computation in the LVish library.  Such a computation
might be just one part of an overall program -- just as ordinary
Haskell values are returned when one runs a Par computation, and the
rest of the program can manipulate or compute with them however one
wishes, the result of evaluating a lambdaLVar/lambdaLVish expression
could be passed to another program to be manipulated.  Adding this
capability to lambdaLVar/lambdaLVish would, we think, be a distraction
from what they are meant to demonstrate.

A threshold set is a way of expressing what kinds of values can be
gotten back from a particular kind of LVar read.  In that sense, it is
more like a type than an ordinary program expression.  The lambdaLVar
and lambdaLVish operational semantics describe an execution phase
during which LVars are modified and read.  Threshold sets are not
actually manipulated during that phase; rather, they would have been
decided upon beforehand, perhaps by a human data structure implementor
who decides what the API to a particular data structure should be,
perhaps by some language operating over lambdaLVar.  Furthermore, we
want to reiterate that threshold sets do not appear anywhere in the
actual LVish library implementation -- not in the program text and not
as runtime data structures.  Rather, they are a mathematical tool for
reasoning about what kinds of LVar reads are safe to do.  Therefore,
we feel that adding machinery for manipulating threshold sets to
lambdaLVar and lambdaLVish would be a distraction.

Finally, the lack of conditional branching is not the fundamental
reason for the determinism guarantee: if it were possible for a get
operation to return a nondeterministic result, then a program that
simply performed a get operation and returned its result would be
nondeterministic.  No conditional branching would be necessary to
expose the nondeterminism.  (In any case, conditional branching that
would allow one to ask whether a particular LVar write has happened
yet is of course not allowed, and this is by design; we cannot do that
without admitting schedule nondeterminism.  Rather than doing that,
the programmer would register a handler on the LVar and then write a
callback function that would run in response to LVar events.  Many of
the examples in section 4 do this.)
