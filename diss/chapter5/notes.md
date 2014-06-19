# Assorted notes

## "Infinitely often"

I understand this MUCH better now.

http://cstheory.stackexchange.com/questions/22155/what-does-it-mean-for-cvrdt-replicas-to-transmit-their-state-infinitely-often

## Causal history states

The states that an entry in a causal history can be in are different
from the states a replica can be in!  The state of an entry in a
causal history is a set of udates, where an update is: a replica, an
execution index, and an argument.

## The SEC proof

There are three conditions that suffice to ensure strong eventual
consistency (henceforth SEC) as described in the paper.  One is
_eventual delivery_, which says that all updates eventually reach all
replicas.  (The other two are _strong convergence_, which says that
replicas that have received the same updates have the same state, and
_termination_, which says that all method executions eventually
terminate.)

In the paper's proof that a CvRDT object is SEC, we _assume_ eventual
delivery and termination, so it's only strong convergence that's left
to prove.  Because we assume eventual delivery, we know that all
updates eventually reach all replicas. Hence all replicas eventually
have equivalent causal histories.

So, suppose a non-inflationary update occurred at some replica and
then its state merged with the others.  Well, we're assuming that that
update reached all the other replicas as well.  So.

I still don't get it.  Suppose a non-inflationary update occurred at
some replica.  Well, we're already assuming that that non-inflationary
update eventually reaches all replicas.

Suppose r1's state is {a, b} and r2's state is {c}.
We do a non-inflationary update to r1, removing b, leaving r1 as {a}.  r1 and
r2 send each other their states, and each one performs a merge,
leaving each as {a, c}.  However, we also assume that the "remove b"
update reaches r2.  What does "remove b" do to {a, c}?  If it leaves
it the same, we're fine.
