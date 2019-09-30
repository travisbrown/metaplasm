---
title: Implicit scope and Cats
date: Mon 30 Sep 2019 03:27:18 AM CDT
tags: scala, fp, cats
---

This post is an attempt to provide some additional context and justification for
[an experiment](https://github.com/typelevel/cats/pull/3043) that I've been working on as a proposal for a
future version of [Cats](https://github.com/typelevel/cats) (probably 3.0). The argument is that by
moving Cats's type class instances for standard library types into implicit scope, we can provide a
better user experience along a couple of dimensions (fewer imports to think about, faster compile
times), while also making the library better aligned with future changes in the language and compiler.

<!-- MORE -->

## Type classes in Scala

In Scala 2, type classes are typically encoded as generic classes or traits, with type class constraints
encoded as implicit parameters. For example, a [monoid](https://en.wikipedia.org/wiki/Monoid) type class implementation might look like this:

```scala
trait Monoid[A] {
  def empty: A
  def combine(a: A, b: A): A
}
```

And a generic method that requires an instance of this type class would look like this:

```scala
def getOrEmpty[A](maybe: Option[A])(implicit A: Monoid[A]): A = maybe.getOrElse(A.empty)
```

We could rewrite this method using a context bound, we could provide it as an extension method on 
`Option`, we could add a `Semigroup` supertype to our type class hierarchy, etc.
There are several [good introductions](https://typelevel.org/cats/typeclasses.html) to type classes in
Scala that deal with topics like these, and this post won't try to be one of them. 
Instead the next few sections will focus on where we can (and should) define instances of our type classes, and on how the compiler finds them.

## Lexical scope

One common choice for where to define our instances is "wherever we want, in some object that users can import from".
You'll often see Scala libraries that do something like this:

```scala
object MonoidInstances {
  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    val empty: Int = 0
    def combine(a: Int, b: Int): Int = a + b
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def empty: List[A] = Nil
    def combine(a: List[A], b: List[A]): List[A] = a ++ b
  }
}
```
Now our users just have to remember to import `MonoidInstances._` and these instances will be available implicitly:

```scala
scala> import MonoidInstances._
import MonoidInstances._

scala> getOrEmpty(Option(123))
res0: Int = 123

scala> getOrEmpty(None: Option[Int])
res1: Int = 0
```
In [Scalaz](https://github.com/scalaz/scalaz/) and [Cats](https://github.com/typelevel/cats) there are a bunch of objects named things like
`int` and `list` and `all` in a `std` (for "standard") or `instances` package object, in [Spray JSON](https://github.com/spray/spray-json) these implicit-holding objects are typically
named something ending in `Protocol`, etc. While the naming
conventions vary, the principle is the same: the library author defines instances in some more or less arbitrarily made-up object,
and the library user has to import those instances in their code.

When users import these instances, they're putting them into _lexical scope_. If the import is at the
top of the source file, `intMonoid` will be available for use by that name everywhere in the file, assuming it's not
[shadowed](https://en.wikipedia.org/wiki/Variable_shadowing) by some subsequent import or definition
(possibly in some more specific scope).

Lexical scope is the first place the Scala compiler looks for implicits. When we write `getOrEmpty(Option(123))`, the
compiler will infer that the type of the argument is `Option[Int]`, so the method's `A` is `Int`, which means it needs
an implicit `Monoid[Int]` value. It will then look through all of the implicit definitions in the current scope for one
that has that type, or a more specific type (if we had a `CommutativeMonoid` class that extended `Monoid` and had used it to define
`intMonoid`, for example).

It's worth noting that the way that importing implicits into lexical scope works is [likely to change in Scala 3](https://dotty.epfl.ch/docs/reference/contextual/import-delegate.html), which will require a new `given` selector for things like type class instances. This is just one of many reasons to avoid relying on importing implicits into lexical scope whenever we can, and instead to use _implicit scope_.

## Implicit scope

If the compiler doesn't find an implicit definition of the right type in lexical scope, it doesn't give up, but looks in
another set of places. Suppose we try the following:

```scala
scala> getOrEmpty(None: Option[Vector[String]])
                 ^
       error: could not find implicit value for parameter A: Monoid[Vector[String]]
```
This doesn't compile because we don't have a monoid instance for `Vector`, but before it failed the
compiler looked at `intMonoid`, `listMonoid`, and then at the implicit definitions in the companion
objects of both `Vector` and `Monoid`, as well as in the companion objects of all of their supertypes.

We can start a new REPL session and redefine the `Monoid` trait as above, but with a new companion object like this:

```scala
object Monoid {
  implicit def vectorMonoid[A]: Monoid[Vector[A]] = new Monoid[Vector[A]] {
    def empty: Vector[A] = Vector.empty
    def combine(a: Vector[A], b: Vector[A]): Vector[A] = a ++ b
  }
}
```

Now if we also redefine our `getOrEmpty` it'll just work for this case:

```scala
scala> getOrEmpty(None: Option[Vector[String]])
res0: Vector[String] = Vector()
```

Note that `vectorMonoid` isn't in lexical scope—we can't refer to it by name:

```scala
scala> vectorMonoid
       ^
       error: not found: value vectorMonoid
```

But because it's in the companion object for `Monoid`, the compiler will find it in an implicit search for a
`Monoid` for any `Vector`, regardless of what we've imported (assuming we haven't imported a different instance that
matches first).

An implicit scope search for a `Monoid[Foo]` will search the companion objects for both `Monoid` and `Foo`, because 
`Foo` might not be available when we define `Monoid` in our library. If we have a `ItemStats` case class in our Scala
application, for example, we can't expect Cats (or whatever library provides our
`Monoid`) to define a `Monoid[ItemStats]`  in the `Monoid` companion object. We _can_ put it in the `ItemStats` companion
object, though:

```scala
case class ItemStats(count: Long)

object ItemStats {
  implicit val itemStatsMonoid: Monoid[ItemStats] = new Monoid[ItemStats] {
    val empty: ItemStats = ItemStats(0)
    def combine(a: ItemStats, b: ItemStats): ItemStats = ItemStats(a.count + b.count)
  }
}
```

Now our `Monoid` instance for `ItemStats` will be available to the compiler anywhere it's needed,
without any imports.

The details of how implicit search works are beyond the scope of this post, but I'd encourage
anyone who's interested to read the [official tutorial](https://docs.scala-lang.org/tutorials/FAQ/finding-implicits.html#where-do-implicits-come-from) on implicit search in Scala.

## Orphan instances

More generally, our type class definitions are often either going to be upstream or downstream from the types we're defining instances for.
For standard library types, the library defining the type classes will be downstream, so standard library instances can
go in the type class companion objects. For user-defined types in a project that depends on our type class library,
the type class definitions will be upstream, and the instances can be defined in the companion objects of the user-defined types.

For either of those cases we can use Scala's implicit scope. Where implicit scope doesn't work is when we have two libraries that
have no dependency in either direction. For example, the [Squants](http://www.squants.com/) library doesn't depend on [Circe](https://github.com/circe/circe),
and Circe doesn't depend on Squants, so if we wanted to provide an `io.circe.Decoder` instance for `squants.space.Length`,
we wouldn't be able to define it in either the `Decoder` or `Length` companion objects. We'd have to fall back to
a separate compatibility module that depended on both, with instances that the user would have to import into lexical scope.

Scala isn't the only language that uses the type class pattern, and other languages also make a distinction between
instances that are defined in the modules of either the type class or the characterized type (Scala's implicit scope),
and instances that are defined somewhere else and have to be imported (Scala's lexical scope). In languages like [Haskell](https://wiki.haskell.org/Orphan_instance) and [Rust](https://internals.rust-lang.org/t/orphan-rules/1322), for example, the latter are called "orphan instances", and their
use is discouraged or not fully supported by the language.

The question of whether orphan instances should be allowed is outside the scope of this post,
which is about Scala 2, where we're stuck with them whether we like it or not.
While there may be good reasons to support orphan instances, they definitely do come with a cost, and in the case of Cats's standard library instances they aren't necessary (at least in theory),
since Cats does depend on the Scala standard library.

## Orphan instances in Cats

So if we think it's a good idea to avoid orphan instances whenever we can, why does Cats (and Scalaz before it) use lexical
scope to provide instances for standard library types? I've [asked this question](https://twitter.com/travisbrown/status/1167308493742735365) many times over the past decade, and the
most sensible answer I've heard is that it's because it can be difficult to know how to arrange instances in complex
type class hierarchies.

(The wackiest answer I've heard is that it's because the Scalaz authors wanted to punish users who chose
standard library types over Scalaz's equivalents, but this seems unlikely for chronological reasons—types like `scalaz.Maybe` came after the decision to use lexical scope for standard library instances.)

We can take the following type class inheritance hierarchy from the `cats.arrow` package as an example (note that all of the diagrams in this post are derived
from Rob Norris's [Cats Infographic](https://github.com/tpolecat/cats-infographic)):

<img width="60%" src="/images/arrow-01.svg" style="margin-right: auto; margin-left: auto; display: block;">

We know we can define `Choice` and `CommutativeArrow` instances for the standard library's `Function1`, but if we want to put these instances
into implicit scope, we have to make some decisions. We can't just put them in their respective companion objects (`Choice` and
`CommutativeArrow`), because then they wouldn't be found during an implicit search for e.g.
`Compose[Function1]` (remember that when
the compiler is looking in implicit scope for a `Compose[Function1]`, it will only search the companion objects of `Compose`,
`Function1`, and their _supertypes_, not their subtypes).

We have a couple of options. We could keep the instances in the maximally specific companion objects and have "upcast" instances in every non-leaf type class, raising
e.g. our `Choice[Function1]` to a `Category[Function1]` in `Choice`, and a `Category` to a `Compose` in `Compose`. This works, but it's a lot of
invasive machinery, and we still have to worry about prioritization for type classes with multiple children.
Alternatively we could put the instances into the type class hierarchy roots, where they'll always be found
by supertype searches, but again we have to worry about prioritization
(because some type classes like `Arrow` and `CommutativeArrow` will descend from multiple roots).


This problem becomes even more complicated for more complex hierarchies. The graph that includes `Monad` and `Traverse`
for example, has dozens of elements, six or seven levels, and four or five roots. Coming up with a strategy for distributing
hundreds of standard library instances across a hierarchy like this is a challenge, and apparently nobody working on Scalaz
or Cats has thought that the advantages of implicit scope for standard library instances would be worth making the effort.

## Our strategy

I'm stubborn, though, and a couple of months ago while I was experimenting with [porting Cats and Circe to Dotty](https://github.com/travisbrown/dotty-experiments)
I decided to try to come up with a principled way to arrange the standard library instances in the Cats type class companion objects. I
ended up going with the roots approach, and I handled prioritization by splitting the inheritance graphs
into proper singly-rooted trees (color-coded here):

<img width="60%" src="/images/arrow-02.svg" style="margin-right: auto; margin-left: auto; display: block;">

To return to our `Function1` example, we know we have `Choice` and `CommutativeArrow` instances
(because those are the `cats.arrow` instances
in our `cats.instances.function` package). Our strategy for deciding where to put these instances is to walk from each specific instance
to each of its roots, and to include the instance there. For example, we'll have the following instance in the `Compose`
companion object:

```scala
object Compose {
  implicit val catsInstancesForFunction1: Choice[Function1] with CommutativeArrow[Function1] = ???
}
```

Since we can reach that root from both our `Choice` and our `CommutativeArrow` instances. We need
one more instance, though, since from `CommutativeArrow` we can also reach the `Profunctor` root. In
that case, though, we cross a boundary from the pink tree into the green tree, and when we do that, we
change the instance we're tracking to the first green type class (in this case `Strong`). This gives
us the following instance for the `Profunctor` companion object:

```scala
object Profunctor {
  implicit val catsStrongForFunction1: Strong[Function1] = ???
}
```

This approach seems to work fairly mechanically, even for the much more complex graph with `Monad` and friends. You have to make some
arbitrary decisions about how to cut up the graph, but Cats is full of arbitrary decisions.

## Current status

I currently have a [pull request](https://github.com/typelevel/cats/pull/3043) that makes all of the type class instances
provided in `cats.instances` available in implicit scope in this manner. The branch is verified to be binary compatible
with Cats 2.0.0, and I've modified the tests to remove all use of `Instances` traits and imports, to show that all of
the instances required by the tests are actually really in implicit scope.

The diff isn't _that_ big (`+925 −118`), and even in the current intermediate form, where `cats.instances` imports are still supported, the cats-core jar is only about 1% larger than for Cats 2.0.0.

I've also published the branch to Maven Central, so you can try it out now:


```scala
scalaVersion := "2.13.1"

libraryDependencies += "dev.travisbrown" %% "cats-core" % "2.0.0-CATS_3043-1"
```

And then:

```scala
scala> cats.Traverse[List]
res0: cats.Traverse[List] = cats.instances.ListInstances$$anon$1@539a4733
```

No imports necessary. These cats-core and cats-kernel artifacts are a drop-in replacement for Cats 2.0.0, and should work
with any other dependencies that depend on Cats 1.x or 2.x, but note that to be safe you should make sure that the original
`org.typelevel` artifacts aren't pulled in transitively:

```scala
scalaVersion := "2.13.1"

libraryDependencies += "dev.travisbrown" %% "cats-core" % "2.0.0-CATS_3043-1"
libraryDependencies += Seq(
  "io.circe" %% "circe-jawn" % "0.12.1",
  "org.typelevel" %% "cats-laws" % "2.0.0"
).map(
  _.excludeAll("org.typelevel" %% "cats-core", "org.typelevel" %% "cats-kernel")
)
```

I've tried building a few libraries like Circe with this dependency, both with and without `instances` imports, and
everything seems to work fine. If you try it and run into any issues, please comment on the [pull request](https://github.com/typelevel/cats/pull/3043).

## Compiler performance

The most obvious advantage of this change is that it means users never have to think about one whole class of imports. For some people this is a big deal—here's one [Reddit commenter](https://www.reddit.com/r/scala/comments/d1o28h/proposal_moving_the_cats_std_lib_type_class/ezq5fmb/), for example:

> For me this change would fix one of the most annoying aspects of using cats (or scalaz).

I'm one of these people. I've spent some time over the past couple of months working with this arrangement,
both in my Dotty port and in my Cats 2 fork, and I've found it to be a surprisingly big quality-of-life improvement.
It's almost strange how nice it is just to be able to open a REPL and start typing…

```scala
scala> cats.Parallel[Either[String, ?]]
res0: cats.Parallel[[β$0$]scala.util.Either[String,β$0$]]{type F[x] = cats.data.Validated[String,x]} = ...
```

…or whatever, without worrying about setting up your environment.

For other people this isn't as much of a selling point, though.
You can use sbt's `initialCommands` option to have the Cats instances automatically imported in your REPL,
and in Scala 2.13 you can even [customize the implicit imports](https://github.com/scala/scala-dev/issues/474)
in your source files. The rest of this post will be about another benefit of the implicit scope approach:
it makes your programs compile faster.

When I started this experiment, I didn't really have intuitions about how the change would affect compile times.
In Cats 2.0.0 on Scala 2.13, `cats.instances.all` contains 489 implicit definitions, and `cats.implicits` has 644.
Putting all of these instances into lexical scope—which is where the compiler starts every implicit search—seemed like it must have a cost, but so does implicit scope search, which requires the compiler to look in many more places (potentially dozens of companion objects).

In the first weekend when I was putting together the Cats pull request, I made a mistake while removing
`cats.instances` from the tests, and I wasn't able to see any differences in compile times.
I've since fixed that mistake, and the following graph shows the difference between the [current version](https://github.com/typelevel/cats/pull/3043/commits/fb8e908999ea0a195258ad7abefa1c2fe2ba5e2e) of the branch
and [Cats 2.x](https://github.com/typelevel/cats/commit/c40a781c2dcff256794157e67cebfe1f9c278170) for `testsJVM/test:compile` after `testsJVM/test:clean`
(after an initial full build). The measurements are in seconds, and smaller values are better.

<div id="compile-times" style="width: 100%; height: 256px;"></div>

The improvement here ranges from 40% to 46%. I've seen similar results for generated files that summon hundreds of instances compiled against Cats 2.0.0 with imports and the `2.0.0-CATS_3043-1` artifacts without.

Compile times are [often cited](https://www.lightbend.com/blog/scala-compile-faster) as one of the worst things
about working in Scala, and this improvement is likely to benefit anyone who uses Cats, especially if they're using the
`implicits` or `instances.all` imports. These imports clutter your lexical scope, and have an impact on all of the implicit
searches that your program requires, even if they don't involve Cats type classes.

I'd really love to see this change happen in Cats 3, and [the pull request](https://github.com/typelevel/cats/pull/3043) goes into more detail about how the change could be made in steps that would ease the migration path for users.
In the meantime I think the branch and forked artifacts are ready for experimentation—there are still likely to be some rough edges, but I'd appreciate help in finding them.

<script src="https://cdn.plot.ly/plotly-1.5.0.min.js"></script>
<script>
var l212 = {
  x: ['Cats 2.0', 'CATS-3043'],
  y: [90.6, 48.3],
  marker:{
    color: '#ffd9eb'
  },
  name: 'Linux; Java 8; Scala 2.12',
  error_y: {
    type: 'data',
    array: [1.316, 1.001],
    visible: true
  },
  type: 'bar'
};
var l213 = {
  x: ['Cats 2.0', 'CATS-3043'],
  y: [92.7, 50.2],
  marker:{
    color: '#ff99ca'
  },
  name: 'Linux; Java 8; Scala 2.13',
  error_y: {
    type: 'data',
    array: [1.179, 0.809],
    visible: true
  },
  type: 'bar'
};
var m212 = {
  x: ['Cats 2.0', 'CATS-3043'],
  y: [77.8, 46.1],
  marker:{
    color: '#acdf87'
  },
  name: 'MacBook Pro; Java 11; Scala 2.12',
  error_y: {
    type: 'data',
    array: [0.459, 0.3689],
    visible: true
  },
  type: 'bar'
};
var m213 = {
  x: ['Cats 2.0', 'CATS-3043'],
  y: [83.4, 48.3],
  marker:{
    color: '#68bb59'
  },
  name: 'MacBook Pro; Java 11; Scala 2.13',
  error_y: {
    type: 'data',
    array: [0.4216, 0.337],
    visible: true
  },
  type: 'bar'
};
var data = [l212, l213, m212, m213];
var layout = {barmode: 'group', margin: {t: 0, b: 40}};
Plotly.newPlot('compile-times', data, layout);
</script>
