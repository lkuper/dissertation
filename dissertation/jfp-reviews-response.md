Thanks to both reviewers for their detailed reading of the paper and
suggestions for improving it.  We've incorporated many of the
reviewers' suggestions.  Here we briefly outline the changes we made.

Reviewer 1 made three major suggestions:

>  1\. Serious thought given to the suggestion of merging lambda_LVar and
>     lambda_LVish into just one calculus, so as to reduce duplication and
>     shorten the paper. The new calculus should be as simple as lambda_LVar and
>     as powerful as lambda_LVish. Is it possible? I would like to think so, but
>     perhaps I am being naive.

>  2\. An explicit presentation of the LVish API, possibly at the cost of
>     consuming extra space;

>  3\. A fuller presentation and a better explanation of the LVish implementation.

Addressing these points one at a time:

>  1\. Serious thought given to the suggestion of merging lambda_LVar and
>     lambda_LVish into just one calculus, so as to reduce duplication and
>     shorten the paper. The new calculus should be as simple as lambda_LVar and
>     as powerful as lambda_LVish. Is it possible? I would like to think so, but
>     perhaps I am being naive.

Combining lambdaLVar and lambda LVish into one calculus with the
simplicity of lambdaLVar and the power of lambdaLVish: having given it
some serious thought, we don't think it's possible to "have our cake
and eat it too" here, because the main thing that complicates the
definition of lambdaLVish is the addition of handlers/quiescence, and
that's also the thing that makes it more powerful than lambdaLVar.

However, in the interest of reducing duplication and shortening the
ppaer, we agree the reviewer that presentation-wise, it's not
necessary to go through all the steps of the (quasi-)determinism proof
twice, once for lambdaLVar and once for lambdaLVish (and reviewer 2
made a similar point).  We've therefore made section 3 significantly
less repetitive.

Furthermore, we've improved the presentation by adding non-idempotent
update operations (rather than just least-upper-bound writes) to the
original calculus, lambdaLVar.  This makes lambdaLVar a little more
complicated, but it makes for a smaller leap from lambdaLVar to
lambdaLVish, and it helps illustrate that the non-idempotent update
operations in lambdaLVish are not the source of its nondeterminism.

lambdaLVish now just adds two new features to the language:
handlers/quiescence, and freezing.  We think this makes sense
pedagogically, because these two features are especially useful in
combination.

The reviewer suggested that we could simplify lambdaLVish by viewing
freeze as just another update operation.  However, since unlike other
update operations, freeze returns a value (it performs a read as well
as a write), it seems different enough from other update operations
that it should be kept separate.  Moreover, it is the one feature that
introduces nondeterminism.  Therefore, we think it makes sense to keep
it separate.

>  2\. An explicit presentation of the LVish API, possibly at the cost of
>     consuming extra space;

We've adopted this suggestion; section 4.2 of the paper now begins
with an explicit presentation of the essential LVish API operations
and types.

>  3\. A fuller presentation and a better explanation of the LVish implementation.

We agree with the reviewer that this section of the originally
submitted paper was hard to understand.  After putting some thought
into this, we've decided that the best approach is to leave discussion
of LVish internals out of this paper entirely.  The code that we
presented in the original version of the paper submitted in December
is already out of date, and the implementation is likely to continue
to evolve. Nor does it do the reader any good to simply repeat the
now-outdated implementation section that appeared in our POPL '14
paper. (For example, the material in that paper about leveraging
idempotency is out of date, now that we allow non-idempotent updates.)

The internals of LVish are more complicated than they used to be, and
still in flux.  We believe the best place to go to read up-to-date
LVish scheduler code is the open-source LVish repository (which is
linked from the paper), not the paper itself.  We hope that the
reviewer is satisfied with this decision, which also helps make up for
the additional space that adding a full presentation of the API takes
up.

Reviewer 2 made five major suggestions.  We will try to summarize each
of these suggestions briefly.

> 1\. *Rather than join-semiattices and lub updates, a focus on posets
> and a more general form of update, starting from early in the
> paper.*

> 2\. *An answer to the question about consistent termination.*

> 3\. *An account of how we deal with early termination and parallel-or.*

We address each of these in order as well:

> 1\. *Rather than join-semiattices and lub updates, a focus on posets
> and a more general form of update, starting from early in the
> paper.*

As noted above, we've adopted the reviewer's suggestion of introducing
non-idempotent updates early in the paper as part of lambdaLVar.
However, we think that requiring a partial function that takes a pair
of states, instead of requiring a join-semilattice with lubs, would
mean a fundamental change to the flavor and focus of the work, which
is about exploiting join-semilattice properties to get
(quasi-)determinism.  Indeed, a join-semilattice isn't always the most
natural fit for the data at hand.  But, excitingly, it often is, and
it's in those settings where LVars are especially useful.  We see
non-idempotent update operations as a useful generalization that
allows us to conveniently handle more kinds of data, such as
incrementable counters.  All that said, we are happy to take some of
the emphasis off least-upper-bound updates, and we hope to accomplish
that by allowing non-idempotent updates in lambdaLVar instead of
waiting until lambdaLVish to introduce them.

> 2\. *An answer to the question about consistent termination.*

TODO.  Issue #7.

> 3\. *An account of how we deal with early termination and parallel-or.*

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

TODO.  Issue #5.

> 5\. I'm still thinking about how to summarize and respond to point 5.
