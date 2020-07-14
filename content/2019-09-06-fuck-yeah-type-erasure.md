+++
title = "Fuck yeah type erasure"
original_date = 2019-09-06T05:37:29
path = "posts/2019/09/06/fuck-yeah-type-erasure"

[taxonomies]
tags = ["scala", "fp", "typelevel", "cats"]
+++

(Apologies for the title—after a lot of time on Twitter this week I've been feeling nostalgic for things like Tumblr c. 2010.)

This post is an attempt to answer a question [Baccata64](https://www.reddit.com/user/Baccata64/) asked on [Reddit](https://www.reddit.com/r/scala/comments/cz4xai/permutive_community_engineering_august_2019_cats/ez3ojmr/) yesterday afternoon:

> how does the Parallel change not break bincompat ? Is it that type parameters and type members are encoded the same way at the bytecode level ?

The context is that [Cats 2.0.0-RC2](https://github.com/typelevel/cats/releases/tag/v2.0.0-RC2) includes a [recent change](https://github.com/typelevel/cats/pull/3012) where the `Parallel` and `NonEmptyParallel` type classes
were changed from having two type parameters each:

```scala
trait NonEmptyParallel[M[_], F[_]] {
  // ...
}
```

…to one, with the parallel context (the `F` parameter) changed to a type member:

```scala
trait NonEmptyParallel[M[_]] {
  type F[_]
  // ...
}
```
This post will give some background about the context and motivation for this change, and then will try to answer Baccata64's question.

<!-- more -->

## Motivation

This change significantly improves usability, since it's generally no longer necessary for users of the
type class to carry an extra type parameter along through all of their definitions that require a
`Parallel` constraint.

Note that the `F[_]` parameter doesn't appear at all in the signatures of most of the commonly used methods on
`Parallel`, either in the argument types or the return type. For example, here's `parTraverse` in
Cats 1.x:

```scala
def parTraverse[T[_]: Traverse, M[_], F[_], A, B](ta: T[A])(f: A => M[B])(implicit P: Parallel[M, F]): M[T[B]]
```

The _only_ place `F` appears (outside the parameter list) is the `Parallel` constraint itself, but the fact that it's there now means
everyone who wants to call `parTraverse` must be able to provide it—and everyone who wants to call methods that call it, etc., all
the way up the call stack.

In Cats 2.x that won't be the case, since the signature looks like this:


```scala
def parTraverse[T[_]: Traverse, M[_], A, B](ta: T[A])(f: A => M[B])(implicit P: Parallel[M]): M[T[B]]
```

It's possible to "hide" the `F` here not only because we typically don't need it (and when we do
there's [the `Aux` pattern](https://stackoverflow.com/a/34548518/334519)), but also because the semantics 
of `Parallel` mean that the `M` always uniquely determines the `F`. For `IO`, the `F` is `IO.Par`. For
`Either[A, *]` it's ` Validated[A, *]]`, etc. We'll never have two distinct implicit instances `Parallel[C] { type P }` and 
`Parallel[C] { type Q }`.

(Or rather we _should_ never have this—strictly speaking nothing prohibits it. Fortunately
if you do happen to get yourself into that situation, in the worst case you're probably just going to have some code that doesn't compile
for confusing reasons.)

Some people have called `Parallel`
["impossible to use"](https://gitter.im/typelevel/cats?at=5d63e222e403470ab6dd1976) in its 1.x state, and
there's [an entire third-party library](https://github.com/ChristopherDavenport/cats-par) dedicated to 
providing a kind of wrapper for `Parallel` that only has a single type parameter.

## Compatibility

That's a not-actually-very-brief explanation of what the change was, and why we made it, but the question we're
actually trying to answer is _how_. Cats 2.x promises binary compatibility on the Java Virtual Machine
with Cats 1.x (at least in most of its modules, including the core module where `Parallel` is defined), and on the face of it,
removing a type parameter seems very much like a breaking change.

In fact I think all of the Cats maintainers (including me)
just kind of assumed that it wouldn't be possible to make this change in 2.0.0, or thought it was unlikely enough to justify making the effort to check,
up until [this moment](https://gitter.im/typelevel/cats?at=5d63de83e403470ab6dd01cd) a couple of weeks ago when I finally got sufficiently
annoyed about the situation:

> wait, it's totally possible to fix `Parallel` in a bincompat way.

[The change](https://github.com/typelevel/cats/pull/3012) definitely does break source compatibility—it breaks source
compatibility very dramatically. Migrating from Cats 1.x to Cats 2.x will
be a chore if you use `Parallel` extensively and directly, which may be the case for some library developers. Fortunately not many adopters are likely to find themselves
in that situation, since most people are probably either not using `Parallel` or using the cats-par wrapper (which has a much
easier migration path).

So we're not too worried about source compatibility breakage in this case. We've tried to minimize it between 1.x minor releases,
but in a few cases it's something we've decided is necessary on a larger scale in 2.0.

Binary compatibility is much more important
(although personally I've [often argued](https://twitter.com/travisbrown/status/1163482681721708544)
that we've prioritized it too highly), and it's important primarily because it allows adopters to upgrade Cats-dependent code
to new versions without requiring all of their other dependencies that also depend on Cats already to have been updated.

To put it another way, we can use the Cats 2.0.0 library jars as a drop-in replacement for any Cats 1.x library
jars; any code that was compiled against Cats 1.x can be used with Cats 2.x jars at runtime.
There are [some details](https://docs.scala-lang.org/overviews/core/binary-compatibility-for-library-authors.html) we can skip over, but this property is generally what we mean by "binary compatibility"—the [Java Language Specification](https://www.oracle.com/technetwork/java/javase/compatibility-417013.html) for example defines it as "preserving the ability to link without error".

## A simplified example

Let's take a simplified Scala example that will stand in for `Parallel`:

```scala
trait Foo[A] {
  def apply: A
}
```

If we compile this code with `scalac Foo.scala` (where `Foo.scala` is our file name), we'll get a `Foo.class` file in
our working directory. We can then inspect the class file with `javap`:

```java
public interface Foo<A> {
  public abstract A apply();
}
```

This looks more or less like a Java translation of our Scala code, and it notably does include the type parameter.

Now we can write some code that uses `Foo`:

```scala
object Bar {
  val foo: Foo[String] = new Foo[String] {
    def apply: String = "hello world"
  }

  def main(args: Array[String]): Unit = println(foo.apply)
}
```
We can compile it in the same way, with `scalac Bar.scala` (the Scala compiler will find our already-built `Foo.class` in the working directory), and then run it:

```
travis$ scala Bar
hello world
```

Next let's edit `Foo.scala`:

```scala
trait Foo {
  type A

  def apply: A
}
```

And run `javap` again (possibly first copying the old `Foo.class` somewhere else if you want to compare the two):


```scala
public interface Foo {
  public abstract java.lang.Object apply();
}
```
We can see that the signatures are different from what we had with the type parameter, but for what we care about this doesn't matter, as we can see by running `Bar` again:

```
travis$ scala Bar
hello world
```

We've not recompiled `Bar`—it was compiled against our first version of `Foo`, but we're still able to use it with the new type parameter-less `Foo`, because for
the purposes we care about, the bytecode is indeed the same.

For what it's worth, you don't have to do this checking by hand. [Lightbend](https://www.lightbend.com/) provides an excellent
tool called [MiMa](https://github.com/lightbend/mima) that can verify that your code is binary-compatible with previous version. You can even make this part of your build validation process via the sbt plugin, which is what Cats and many other libraries do.

## Type erasure

In the example above `Bar` works with the parameterless `Foo` at runtime because of _type erasure_.
People who work in Scala or Java often complain about type erasure, and it does complicate some things that
seem like they should be simple. For example, the following is something that might seem like it should just
obviously work:

```scala
def format[A](value: A): String = value match {
  case x: String => x
  case os: List[Option[String]] => os.flatten.mkString
  case xs: List[String] => xs.mkString
}
```

But instead it crashes at runtime for some inputs:

```scala
scala> format(List("a", "b", "c"))
java.lang.ClassCastException: java.lang.String cannot be cast to scala.collection.IterableOnce
  at scala.collection.StrictOptimizedIterableOps.strictOptimizedFlatten(StrictOptimizedIterableOps.scala:170)
  at scala.collection.StrictOptimizedIterableOps.strictOptimizedFlatten$(StrictOptimizedIterableOps.scala:167)
  at scala.collection.immutable.List.strictOptimizedFlatten(List.scala:82)
  ...
```

We get this exception because the match happens at runtime, and the runtime can't tell the difference between a `List[Option[String]]`
and a `List[String]` because of type erasure, so our `List[String]` hits the second case, not the third, which results in some internal casts failing. To be fair, the Scala compiler did give us a very clear warning for this code before we get to this point (which I skipped over to avoid ruining the surprise), so the runtime failure is kind of our fault.

While this may seem like a serious limitation, not being able to inspect types at runtime is arguably exactly what we want, at least if we've bought into the doctrine of static types and functional programming. I've written about type erasure [many](https://stackoverflow.com/a/55187427/334519) [times](https://stackoverflow.com/a/41904986/334519) [before](https://stackoverflow.com/a/55185535/334519), and so have [many other people](https://stackoverflow.com/q/20918650/334519), and discussions of the benefits of type erasure generally focus on [_parametricity_](https://en.wikipedia.org/wiki/Parametricity), which is a property of functions in purely functional programming languages that's the foundation of many of the benefits of these languages. (Or at least that's what advocates of statically-typed functional programming say. I'm personally one of these advocates—I think parametricity helps me reason about my code and write programs more confidently.)

What the example of this `Parallel` fix shows, though, is that type erasure has benefits that aren't just this impressionistic
"it helps me reason about my code" stuff. In this case we were trying to solve a real-world problem
(an API design mistake) while operating under real-world constraints (binary compatibility guarantees), and we were
able to get the result we wanted (a better user experience in Cats 2.0) entirely thanks to type erasure.
