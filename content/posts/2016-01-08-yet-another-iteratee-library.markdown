---
title: Yet another iteratee library
date: Sun Nov  8 11:29:58 EST 2015
tags: scala, haskell, cats, iteratees, scalaz
---

I'll start with the story of how I got saved, since it's kind of relevant. Back when I was an
English Ph.D. student, I spent a lot of time working on projects that involved natural language
processing, which meant doing a lot of counting trigrams or whatever in tens of thousands of text
files in giant messy directory trees. I was working primarily in Ruby at the time, after years
and years of Java, and at least back in 2008 it was a pain in the ass to do this kind of thing in
either Ruby or Java. You really want a library that provides the following features:

1. Resource management: you don't want to have to worry about running out of file handles.
2. Streaming: you shouldn't ever have to have all of the data in memory at once.
3. Fusion: two successive mapping operations shouldn't need to traverse the data twice.
4. Graceful error recovery: these tasks are all off-line, but you still don't want to have to
   restart a computation that's been running for ten minutes just because the formatting in one file
   is wrong.

Maybe there was such a library for Ruby or Java back then, but if there was I didn't know about it.
I did have some experience with Haskell, though, and at some point in 2010 I heard about
[iteratees][wikipedia-iteratee], and they were exactly what I'd always wanted. I didn't really
understand how they worked at first, but with [iteratee][hackage-iteratee] (and later
John Millikin's [enumerator][hackage-enumerator]) I was able to write code that did what I wanted
and didn't make me think about stuff I didn't want to think about. I started picking Haskell
instead of Ruby for new projects, and that's how I accepted statically-typed functional programming
into my life.

<!-- MORE -->

## Iteratees in Scala

Eventually I started using more Scala than Haskell, and one of the things I always missed in Scala
was a good iteratee library. The [implementation in Scalaz 6][scalaz-6-iteratee] was kind of a mess,
at least compared to the two most popular Haskell implementations, and using it with `scalaz.effect`
for I/O was incredibly slow. Since then scalaz-iteratee has been [overhauled][scalaz-iteratee] and
[play-iteratee][play-iteratee] and [scalaz-stream][fs2] (now "fs2") got created, but I
never really found any of them as satisfying as the iteratee libraries in Haskell I was used to.

Even after the rewrite in Scalaz 7, scalaz-iteratee is still extremely slow, with an inconsistent
API and lots of bugs—e.g. here's about the most basic thing you can imagine doing with `iterate`:

```scala
scala> import scalaz._, Scalaz._, scalaz.iteratee._
import scalaz._
import Scalaz._
import scalaz.iteratee._

scala> (Iteratee.take[Int, List](5) &= EnumeratorT.iterate(_ + 1, 0)).run
res0: scalaz.Id.Id[List[Int]] = List(0, 0, 0, 0, 0)
```

[Nobody seems to care much][scalaz-1068] about these issues (probably because everyone who might care is off working on
[fs2][fs2]).

[Play's iteratee implementation][play-iteratee] is a big improvement over Scalaz's in many ways—it actually feels like people
actually use it, for one thing—but it has several serious limitations for me. Most
importantly, it's not generic over the computational context that processing happens in—you can use
any monad you like as long as the monad you like is the standard library's future. I also just don't
prefer the way it mixes purity and impurity—e.g. I've gotten burned a couple of times because I
didn't know or remember that the enumerator you get from `enumerate` is side-effecting
and not reusable. It's still pretty good, though—I've chosen it over Scalaz's iteratees or nothing fairly often (given that I don't use
Play), especially before Scalaz Stream came around.

## Scalaz Stream

Scalaz Stream isn't an iteratee implementation, but it does the same kind of stream-y, ARM-y stuff,
and the model is similar. I like Scalaz Stream a lot—I've used it for a handful of side projects,
wrote [a little port of Haskell's split library][syzygist] for it, etc. I still find it extremely
complicated, though. I've repeatedly run into issues with e.g. [`gatherMap`][gather-map] and I still
don't have a clear sense of whether I was seeing bugs or misusing the library. Using exceptions for
flow control makes me nervous. The variance gymnastics make my eyes bleed. And so on.

Building a library around the fact that `Nothing` and `Any`
are (accidentally?) kind-polymorphic in Scala is great ([I love that kind of thing][roll-your-own]),
but it feels like the opposite of the simplicity of iteratees. One of the things I like most about
iteratees is that if you've heard the word "monad" before, you've got a reasonable shot at
understanding the basic model in an afternoon. This just isn't the case with Scalaz Stream (or fs2,
from what I've seen so far).

## Introducing [iteratee.io][iteratee-io]

A few months ago I was starting to think about using Scalaz Stream for streaming JSON parsing in
[circe][circe], but then Twitter very generously decided to
[pay me to take a long vacation][twitter-layoffs], and it seemed reasonable to use some of my new free time
to write the Scala iteratee library I'd always wished existed. I'd already [ported the Scalaz
implementation][scalaz-port] to [Cats][cats], and I was happy enough with the way that had gone to
take it as a starting point, so last month I published my port as an 0.1 release and set myself the
following goals for 0.2:

1. Usage should look at least as clean as Scalaz Stream.
2. Performance should be within an order of magnitude of the standard collections library.

An 0.2.0 snapshot of this new library ([iteratee.io][iteratee-io]) is available now, and I'll be
publishing a release as soon as Cats 0.4.0 is out. The rest of this post is an introduction to the
library, with some discussion of how it differs from other iteratee implementations, what the performance
looks like, and my longer-term goals for the project.

## Iteratee basics

There are three important types in any iteratee implementation:

1. Enumerators, which are sources of data.
2. Iteratees, which are computations that reduce streams of data to a value (think sum, length, fold, etc.).
3. Enumeratees, which are computations that transform streams of data (think map or filter).

These types can be combined in various ways:

* An iteratee can be applied to an enumerator, resulting in a new iteratee.
* An iteratee can draw data through an enumeratee, also resulting in a new iteratee.
* Enumerators can be concatenated, resulting in a bigger enumerator.
* An enumerator can be "wrapped" with an enumeratee to create a new enumerator.
* Iteratees can be composed sequentially.
* Enumeratees can be composed sequentially.

And so on.

All three types are parametrized on a computational context (usually monadic, if you want to do
anything interesting), and none of the
operations in the list above actually performs any processing—they simply define new computations.
There's exactly one way to make processing happen: you "run" an iteratee, which results in a value
in the iteratee's monadic context.

## An example

We can make this more concrete with a simple example. We'll solve the first
[Euler problem][euler-1], which asks for the sum of the natural numbers smaller than a thousand that
are multiples of three or five.

```scala
import io.iteratee.pure._

val naturals = iterate(1)(_ + 1)
val mult3or5 = filter[Int](i => i % 3 == 0 || i % 5 == 0)
val multsUnder1000 = takeWhile[Int](_ < 1000).through(mult3or5).map(_.sum)
```

`naturals` is an enumerator (in this case representing an infinite stream), `mult3or5` is an
enumeratee that filters the numbers we care about, and `multsUnder1000` is an iteratee representing
a computation that performs this filtering for the relevant part of the stream and takes the sum of
the resulting values.

Now we can apply the iteratee to our source and run the resulting computation:

```scala
scala> val computation = multsUnder1000(naturals)
computation: io.iteratee.Iteratee[cats.Id,Int,Int] = io.iteratee.Iteratee@3d111cde

scala> computation.run
res0: cats.Id[Int] = 233168
```

There's also shorthand for this, since applying an iteratee to an enumerator and running it is
something you do all the time:

```scala
scala> naturals.run(multsUnder1000)
res1: cats.Id[Int] = 233168
```

As a side note, stack safety is BYOB, so you generally don't want to use `Id` as your monad
outside of simple examples like this. The library provides "modules" for several monads, including
`Id` (which we get above from `io.iteratee.pure`), `Eval` (which _is_ stack-safe), and
Scalaz's `Task` (in a separate task subproject). You just import the contents of the module for
the monad you wish to work in, and you get a bunch of useful enumerators, enumeratees, and
iteratees (most of which have names that look like methods from collection library classes and
companion objects). For example, using the module for the `Eval` monad this time:

```scala
scala> import io.iteratee.eval._
import io.iteratee.eval._

scala> iterate("a")(_ + "a")
res0: io.iteratee.Enumerator[cats.Eval,String] = io.iteratee.Enumerator$$anon$15@335372c9

scala> map[Int, String](_.toString)
res1: io.iteratee.Enumeratee[cats.Eval,Int,String] = io.iteratee.Enumeratee$$anon$7@4216e8c6

scala> fold[Int, List[Int]](Nil)(_ :+ _)
res2: io.iteratee.Iteratee[cats.Eval,Int,List[Int]] = io.iteratee.Iteratee@6d848fab
```

In general, an operation that would be a method on e.g. the `List` companion object is an enumerator
here, while operations that are methods on `List` instances are either enumeratees (if they return
another list) or iteratees (if they return a single value).

## So what?

This probably isn't terribly interesting yet—we're just doing stuff that we could do just as easily
with the standard library's `Stream`. The nicest part is probably the automatic resource management.
Suppose for example that I want to know how many times I've written "flatMap" in the source for the 
core and task subprojects of this library. The task subproject provides enumerators for listing
directory contents and reading lines from files that make this easy:

```scala
import io.iteratee._, io.iteratee.task._, java.io.File, scalaz.concurrent.Task

val words: Enumerator[Task, String] =
  listAllFiles(new File("core/src")).append(listAllFiles(new File("task/src")))
    .flatMap(lines)
    .flatMap(line => enumVector(line.split("\\W").toVector))
```

Now we've got an enumerator that will produce every word in these source trees. We can then count
the flatMaps:

```scala
scala> val countingComputation = words.mapE(filter(_ == "flatMap")).run(length)
countingComputation: scalaz.concurrent.Task[Int] = scalaz.concurrent.Task@5d1fe005

scala> countingComputation.run
res0: Int = 72
```

We could also ask for the first five words that start with "e":

```scala
scala> words.mapE(filter(_.startsWith("e"))).run(take(5)).run
res1: Vector[String] = Vector(exists, experiments, element, extends, elements)
```

It's also possible to describe two computations and zip them together so that they'll both be
computed on a single pass through the stream. For example, if we rephrase our two computations a bit,
we can have them run together:

```scala
scala> val count = length[String].through(filter(_ == "flatMap"))
count: io.iteratee.Iteratee[scalaz.concurrent.Task,String,Int] = io.iteratee.Iteratee@744dbfe

scala>  val es = take[String](5).through(filter(_.startsWith("e")))
es: io.iteratee.Iteratee[scalaz.concurrent.Task,String,Vector[String]] = io.iteratee.Iteratee@6dbf8bba

scala> words.run(count.zip(es)).run
res2: (Int, Vector[String]) = (72,Vector(exists, experiments, element, extends, elements))
```

We don't have to worry about resource management in any of these cases because the `words` enumerator is doing that for
us. When we run the computation, it keeps track of which files it opens, and it will always close
them, even if we don't use all of the words, if an `IOException` gets thrown, etc. It also won't
open files unnecessarily—if we ask for the first five words that start with "e" and they occur in the first file, then
that's the only file that will be opened.

## What about performance?

Right now the performance situation is looking better than I'd hoped—I'm well within an order of
magnitude of the standard collection library. In the following benchmarks `I` stands for this library, `S` for
Scalaz Stream, `Z` for scalaz-iteratee, `P` for play-iteratee, and `C` for the collection library.
Each benchmark is intended to reflect idiomatic use of the library it uses. For iteratee.io, Scalaz
Stream, and scalaz-iteratee, all computations are in the `Task` monad. 

The source for these benchmarks is [available in the GitHub repository][benchmarks], and of course
I'd welcome improvements for any of the implementations.

In the first benchmark we take the sum of 10,000 integers in an in-memory collection (if such a
thing is available for the library). iteratee.io has an advantage over the Scalaz libraries here
because it supports chunked inputs, and it actually beats the standard library, since `sum`
on a vector (which uses Scala's `Numeric`) is slower than the `foldLeft` and `combine` on Algebra's monoid
that the iteratee.io operation ends up using on the chunk.

```
Benchmark                       Mode  Cnt      Score    Error  Units
InMemoryBenchmark.sumIntsI     thrpt   80  15091.100 ± 57.792  ops/s
InMemoryBenchmark.sumIntsS     thrpt   80     78.130 ±  0.866  ops/s
InMemoryBenchmark.sumIntsZ     thrpt   80    309.367 ±  1.130  ops/s
InMemoryBenchmark.sumIntsP     thrpt   80     54.719 ±  1.436  ops/s
InMemoryBenchmark.sumIntsC     thrpt   80  13024.974 ± 22.356  ops/s
```

The results report throughput, so higher numbers are better.

In the second benchmark we collect the first 10,000 values from an infinite stream of non-negative
long integers into a sequence.

```
Benchmark                       Mode  Cnt      Score    Error  Units
StreamingBenchmark.takeLongsI  thrpt   80   1146.021 ±  6.539  ops/s
StreamingBenchmark.takeLongsS  thrpt   80     65.916 ±  0.182  ops/s
StreamingBenchmark.takeLongsZ  thrpt   80    198.919 ±  2.097  ops/s
StreamingBenchmark.takeLongsP  thrpt   80      1.447 ±  0.082  ops/s
StreamingBenchmark.takeLongsC  thrpt   80   3286.878 ± 37.967  ops/s
```

In both benchmarks iteratee.io is within a factor of three of the standard library, and at least an
order of magnitude faster than the other libraries.

To be fair, in many cases these differences will be irrelevant, since the network or disk will be
the bottleneck. My goal is to make it possible and attractive to use the same tools for most or all
of the collections-like operations in your program, though, so that you don't have to switch between
your stream library's combinators and standard collection library operations for the sake of
performance (or anything else).

## The model

With most iteratee libraries it's very easy to construct "bad" iteratees that violate the monad laws.
For example, here's Haskell's [iteratee][hackage-iteratee]:

```haskell
Prelude> import Data.Iteratee
Prelude Data.Iteratee> let leftover = idone () $ Chunk [0]
Prelude Data.Iteratee> let ended = idone () $ EOF Nothing
Prelude Data.Iteratee> run $ (leftover >> ended) >> stream2list
[0]
Prelude Data.Iteratee> run $ leftover >> (ended >> stream2list)
[]
```

So much for bind's associativity. One of my goals for iteratee.io was to make it harder to end up
with these bad iteratees. As part of this effort (and for the sake of performance), some of the work
that's normally done by the "input" representation is instead done by the "step" representation in
iteratee.io.

Usually the input type is an ADT that's either an end-of-stream signal or a collection (possibly empty)
of elements. In iteratee.io, the input type is isomorphic to a non-empty list—there's no end-of-stream
input and no empty input—and the step type has three states instead of the usual two, with the
"extra" state represent an iteratee that's finished with no leftover input.

It's still possible to violate the monad laws:

```scala
scala> import cats.syntax.flatMap._, io.iteratee.eval._
import cats.syntax.flatMap._
import io.iteratee.eval._

scala> ((head[Int] >> done((), Vector(0))) >> head).run.value
res0: Option[Int] = None

scala> (head[Int] >> (done((), Vector(0)) >> head)).run.value
res1: Option[Int] = Some(0)
```

But there are fewer ways for this to happen, and if you don't inject values into
the stream by creating finished iteratees with leftovers they didn't actually
receive, you're safe.

So far this approach seems to work well for my use cases, but it's still an experiment and is subject
to change.

## Other stuff

The creation of iteratee.io was motivated in part by the need for streaming parsing in circe, and it follows
the [design guidelines][circe-design] I wrote for circe. The 0.3.0 release of circe will include a
[circe-streaming][circe-streaming]
module with generic enumeratees for streaming JSON processing—you bring your own `MonadError` and enumerators and
you get nice, fast, streaming parsing and decoding (powered by [Jawn][jawn]'s asynchronous parser).
You can currently build this subproject yourself, and
there's a [circe tutorial][circe-sf-lots] with examples of how you can use it, but it's not yet in the circe 0.3.0 snapshots.

iteratee.io is a small library, with the iteratee-core jar currently weighing in at 324k, and its only runtime
dependency is cats-core. Neither of these things are likely to change in the near future, although I'm planning to add
several new subprojects which may have additional dependencies (for I/O operations in the standard library's `Future`,
more generic zipping with Shapeless, etc.).

Test coverage for [iteratee.io][iteratee-io] is currently at 100%, in part thanks to [Discipline]-powered law
checking, and my goal is to keep 100% coverage a requirement for at least the core project. Oh, and iteratee.io supports and is
published for [Scala.js][scala-js], if you're into that kind of thing.

The library is still very young, but I think it's stable enough to play with, and the API isn't likely to change much
before 0.2.0. If you have any questions, there's a new [Gitter channel][gitter] for the project, or you can file an
issue on [GitHub][iteratee-io] or contact me [on Twitter][twitter].

[benchmarks]: https://github.com/travisbrown/iteratee/blob/master/benchmark/src/main/scala/io/iteratee/benchmark/Benchmark.scala
[cats]: https://github.com/non/cats/tree/master/core/src/main/scala/cats
[circe]: https://github.com/travisbrown/circe
[circe-streaming]: https://github.com/travisbrown/circe/tree/master/streaming/src/main/scala/io/circe/streaming
[circe-design]: https://github.com/travisbrown/circe/blob/master/DESIGN.md
[circe-sf-lots]: https://github.com/travisbrown/circe/tree/master/examples/sf-city-lots
[discipline]: https://github.com/typelevel/discipline
[euler-1]: https://projecteuler.net/problem=1
[fs2]: https://github.com/functional-streams-for-scala/fs2
[hackage-enumerator]: https://hackage.haskell.org/package/enumerator
[hackage-iteratee]: https://hackage.haskell.org/package/iteratee
[iteratee-io]: https://github.com/travisbrown/iteratee
[gather-map]: https://groups.google.com/forum/#!search/scalaz-stream$20eugene/scalaz/ExV2JX6vFfY/5Gdy_-XmcmkJ
[gitter]: https://gitter.im/travisbrown/iteratee
[jawn]: https://github.com/non/jawn
[play-iteratee]: https://www.playframework.com/documentation/2.5.x/Iteratees
[roll-your-own]: https://meta.plasm.us/posts/2015/07/11/roll-your-own-scala/
[scala-js]: http://www.scala-js.org/
[scalaz-iteratee]: https://github.com/scalaz/scalaz/tree/series/7.3.x/iteratee/src/main/scala/scalaz/iteratee
[scalaz-port]: https://github.com/travisbrown/iteratee/commit/ca2a54d949599de1062b34a7cd96707fa729d1bf
[scalaz-1068]: https://github.com/scalaz/scalaz/issues/1068
[scalaz-6-iteratee]: https://github.com/scalaz/scalaz/blob/series/6.0.x/core/src/main/scala/scalaz/Iteratee.scala
[syzygist]: https://github.com/travisbrown/syzygist
[twitter]: https://twitter.com/travisbrown
[twitter-layoffs]: https://meta.plasm.us/posts/2015/10/13/goodbye-twitter/
[wikipedia-iteratee]: https://en.wikipedia.org/wiki/Iteratee
