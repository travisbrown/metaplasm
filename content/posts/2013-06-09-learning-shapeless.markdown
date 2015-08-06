---
title: Learning Shapeless
date: Sun Jun  9 14:07:47 EDT 2013
tags: shapeless, scala
---

I began writing this post as an answer to [this Stack Overflow question](https://stackoverflow.com/q/17009675/334519) about
learning how to use [Shapeless](https://github.com/milessabin/shapeless),
but it ended up a little long for a Stack Overflow answer, so I'm posting it here instead.

When I first started teaching myself type-level programming in the context of Shapeless last year,
I spent a lot of time working through simple problems with heterogeneous lists of type-level natural numbers.
One fairly straightforward (but still non-trivial)
example of such a problem is the second user story of [this Coding Dojo kata](http://codingdojo.org/cgi-bin/wiki.pl?KataBankOCR),
which is also outlined by [Paul Snively](https://twitter.com/psnively) in
[this email](https://groups.google.com/forum/#!msg/shapeless-dev/Q0VezBW2bhQ/RKF6uGljwroJ) to the Shapeless mailing list.

The goal is to determine whether a list of numbers is the appropriate length (nine) and has a valid checksum, which is calculated by taking the sum of each element multiplied by its distance (plus one) from the end of the list, modulo eleven.

<!-- MORE -->

Writing a value-level implementation is easy:

``` scala
def checksum(l: List[Int]): Int = l.reverse.zipWithIndex.map {
  case (v, i) => v * (i + 1)
}.sum % 11

def isValid(l: List[Int]): Boolean = l.size == 9 && checksum(l) == 0
```

We can confirm that this works as desired:

``` scala
scala> isValid(List(3, 4, 5, 8, 8, 2, 8, 6, 5))
res0: Boolean = true

scala> isValid(List(3, 1, 5, 8, 8, 2, 8, 6, 5))
res1: Boolean = false
```

Suppose we want to write a type-level version instead—something like this:

``` scala
def isValid[L <: HList] = ???
```

Where this will compile:

``` scala
isValid[_3 :: _4 :: _5 :: _8 :: _8 :: _2 :: _8 :: _6 :: _5 :: HNil]
```

But this won't:

``` scala
isValid[_3 :: _1 :: _5 :: _8 :: _8 :: _2 :: _8 :: _6 :: _5 :: HNil]
```

This is possible, but it requires an approach that's a little different from the one we took in our value-level implementation.
We'll start by defining a type class that witnesses that a list `L` of type-level naturals has a specific checksum `S` (which is also a type-level natural):

``` scala
import shapeless._, nat._, ops.hlist.Length, ops.nat.{ Mod, Prod, Sum }

trait HasChecksum[L <: HList, S <: Nat]
```

Note that I'm using Shapeless 2.2.5—see [the mailing list thread](https://groups.google.com/forum/#!msg/shapeless-dev/Q0VezBW2bhQ/RKF6uGljwroJ)
mentioned above for a version that works with Shapeless 1.2.4. Also note
that you will have to wait a _very_ long time for Scala 2.11 to come up with
`HasChecksum` instances for lists with more than seven elements (at least through 2.11.7; we're [working on this](https://twitter.com/milessabin/status/629377999926902784)).
The examples below should compile on Scala 2.10.5 in a few seconds, though.

Now we'll tell the compiler how to build up instances of this type class inductively. We start with our base case, the empty list:

``` scala
implicit object hnilHasChecksum extends HasChecksum[HNil, _0]
```

This is pretty simple—we're just providing implicit evidence that the checksum of the empty list is zero.

Next we need to deal with the case `H :: T`, where `H` is a type-level natural and `T` is some list that we already know the checksum of. This is a little more complicated, so we'll work in steps. First for the basics:

``` scala
implicit def hlistHasChecksum[H <: Nat, T <: HList, S <: Nat] = new HasChecksum[H :: T, S] {}
```

We've also said that we know the checksum of `T`, which we can formalize like this:

``` scala
implicit def hlistHasChecksum[H <: Nat, T <: HList, S <: Nat, TS <: Nat](
  implicit st: HasChecksum[T, TS]
) = new HasChecksum[H :: T, S] {}
```

We also need to know the length of `T` in order to compute the checksum of `H :: T`,
so we'll ask the compiler to find an instance of `Length.Aux[T, TL]`.
`Length.Aux` is a type class provided by Shapeless that plays a role similar
to that of our `HasChecksum`—it just provides evidence that some `T: HList`
has length `TL <: Nat`.
(Note that this also means we need to add a new type parameter `TL <: Nat`.)

``` scala
implicit def hlistHasChecksum[
  H <: Nat, T <: HList, S <: Nat,
  TS <: Nat, TL <: Nat
](
  implicit
  st: HasChecksum[T, TS],
  tl: Length.Aux[T, TL]
) = new HasChecksum[H :: T, S] {}
```

Now we can take this product of `H` and the length of `T` plus one:

``` scala
implicit def hlistHasChecksum[
  H <: Nat, T <: HList, S <: Nat,
  TS <: Nat, TL <: Nat,
  HP <: Nat
](
  implicit
  st: HasChecksum[T, TS],
  tl: Length.Aux[T, TL],
  hp: Prod.Aux[H, Succ[TL], HP]
) = new HasChecksum[H :: T, S] {}
```

And add the result to the checksum for `T`:

``` scala
implicit def hlistHasChecksum[
  H <: Nat, T <: HList, S <: Nat,
  TS <: Nat, TL <: Nat,
  HP <: Nat, HS <: Nat
](
  implicit
  st: HasChecksum[T, TS],
  tl: Length.Aux[T, TL],
  hp: Prod.Aux[H, Succ[TL], HP],
  hs: Sum.Aux[HP, TS, HS]
) = new HasChecksum[H :: T, S] {}
```

And finally take the remainder of dividing that result by eleven:

``` scala
implicit def hlistHasChecksum[
  H <: Nat, T <: HList, S <: Nat,
  TS <: Nat, TL <: Nat,
  HP <: Nat, HS <: Nat
](
  implicit
  st: HasChecksum[T, TS],
  tl: Length.Aux[T, TL],
  hp: Prod.Aux[H, Succ[TL], HP],
  hs: Sum.Aux[HP, TS, HS],
  sm: Mod.Aux[HS, _11, S]
) = new HasChecksum[H :: T, S] {}
```

Now we have all the pieces we need to write our type-level `isValid`,
which doesn't even need a body, since all we care about is the fact that the
compiler can assemble the appropriate pieces of evidence:

``` scala
def isValid[L <: HList](implicit l: Length.Aux[L, _9], c: HasChecksum[L, _0]) = ()
```
 
And we're done:

``` scala
scala> isValid[_3 :: _4 :: _5 :: _8 :: _8 :: _2 :: _8 :: _6 :: _5 :: HNil]

scala> isValid[_3 :: _1 :: _5 :: _8 :: _8 :: _2 :: _8 :: _6 :: _5 :: HNil]
<console>:20: error: could not find implicit value for parameter c ...
```

See [this thread](https://groups.google.com/d/msg/shapeless-dev/Q0VezBW2bhQ/4E6GjYQcKhkJ)
on the Shapeless mailing list for some additional discussion,
as well as [this presentation](http://www.infoq.com/presentations/Types-Tests)
from last year's [Strange Loop](https://thestrangeloop.com/),
where Paul Snively presents [my original implementation](https://gist.github.com/travisbrown/3763016) as one of his examples of the power of type-level programming.

