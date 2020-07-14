+++
title = "Why Parallel"
original_date = 2019-09-11T02:00:26
path = "posts/2019/09/11/why-parallel"

[taxonomies]
tags = ["scala", "fp", "typelevel", "cats"]
+++

I've written a couple of [blog](https://meta.plasm.us/posts/2019/09/06/fuck-yeah-type-erasure/)
[posts](https://meta.plasm.us/posts/2019/09/10/cats-2.0-migration/)
about how the `Parallel` type class has changed in
[Cats 2.0](https://github.com/typelevel/cats/releases/tag/v2.0.0),
but those posts don't really say much about why someone using Cats should care about `Parallel` in the first
place. The name suggests that it has something to do with running computations at the same time, and
while that's one of things you can do with it (via the instance for `IO` in
[cats-effect](https://github.com/typelevel/cats-effect), for example), it has a much, much wider
range of applications. This post will focus on a real-world use case for `Parallel` that at a glance
might not seem to have much in common with running things in parallel: accumulating errors while validating form inputs.

<!-- more -->

## Example problem

This morning I remembered a [Stack Overflow question](https://stackoverflow.com/q/20065853/334519)
about validation in Scala that I asked almost six years ago. In the question I give an example where I have a list of pairs of
strings, and I want to parse all of the strings into integers, while also verifying that the second
number in each pair is larger than the first. So the following would be valid input (in CSV form):

```csv
1, 2
-100, 100
200, 3000
```

While this would not:

```csv
a, 1
b, c
1, 0
```

I want to be able to present a complete list of problems to the person who submitted the data, so I
need to be able to accumulate errors while parsing and validating. In the second data set, for
example, there are four errors: three non-integer strings and one wrongly ordered pair.

## Without Parallel

Suppose I have some Scala code for parsing numbers and validating the pairs:

```scala
import scala.util.Try

case class InvalidSizes(x: Int, y: Int) extends Exception(
  s"Error: $x is not smaller than $y!"
)

def parseInt(input: String): Either[Throwable, Int] = Try(input.toInt).toEither

def checkValues(p: (Int, Int)): Either[InvalidSizes, (Int, Int)] =
  if (p._1 >= p._2) Left(InvalidSizes(p._1, p._2)) else Right(p)
```

I can then compose these methods using operations from Cats type classes like `Traverse` (my
original Stack Overflow question used [Scalaz](https://github.com/scalaz/scalaz), but in this post
I'll translate):

```scala
import cats.data.EitherNel
import cats.instances.either._
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.traverse._

def checkParses(p: (String, String)): EitherNel[Throwable, (Int, Int)] =
  (parseInt(p._1).toValidatedNel, parseInt(p._2).toValidatedNel).tupled.toEither

def parse(input: List[(String, String)]): EitherNel[Throwable, List[(Int, Int)]] =
  input.traverse(
    checkParses(_).flatMap(checkValues(_).toEitherNel).toValidated
  ).toEither
```

This solution works just fine:

```scala
scala> val badInput = List(("a", "1"), ("b", "c"), ("1", "0"))
badInput: List[(String, String)] = List((a,1), (b,c), (1,0))

scala> parse(badInput).leftMap(_.toList.foreach(println))
java.lang.NumberFormatException: For input string: "a"
java.lang.NumberFormatException: For input string: "b"
java.lang.NumberFormatException: For input string: "c"
InvalidSizes: Error: 1 is not smaller than 0!
```

The problem that I'm complaining about in this ancient Stack Overflow question is that all of these conversions
between `Either` and `Validated` feel inelegant:

> I bounce back and forth between `ValidationNel` and `\/` [Scalaz's `Either`] as appropriate depending on whether I need error accumulation or monadic binding.

I didn't know it at the time, but I was asking for `Parallel`, which as far as I can tell was first
introduced into a library in a mainstream language a [year later](https://github.com/purescript/purescript-parallel/commit/97f6f140e237a9528e776630e77fe4704fafb5c6).


## With Parallel

The `Parallel` type class really isn't anything more than a way to generalize this process of
going back and forth between monadic and applicative contexts. It allows us to rewrite our
`checkParses` and `parse` methods without ever referring to `Validated`:

```scala
import cats.data.EitherNel
import cats.instances.either._
import cats.instances.list._
import cats.instances.parallel._
import cats.syntax.either._
import cats.syntax.parallel._

def checkParses(p: (String, String)): EitherNel[Throwable, (Int, Int)] =
  (parseInt(p._1).toEitherNel, parseInt(p._2).toEitherNel).parTupled

def parse(input: List[(String, String)]): EitherNel[Throwable, List[(Int, Int)]] =
  input.parTraverse(checkParses(_).flatMap(checkValues(_).toEitherNel))
```

These do exactly the same thing as the versions above, but instead of having to convert `Either`
to `Validated` manually when we want error accumulation (and then convert back to `Either` when we
want monadic binding), all we have to do is use the `par` versions of `tupled` and `traverse`.

These parallelized operations are available here because `Either[E, ?]` has a `Parallel` instance
(assuming we have a `Semigroup` for `E`). This instance encodes the fact that we can
convert `Either` values to `Validated` values and then use applicative operations on those values
to get error accumulation, but it hides all of that from the user, who only has to know that
_somehow_ validation is being done in parallel instead of sequentially (where it would fail fast).

This is a pattern that comes up often. To give one other example, `IO.Par` in cats-effect is a version of `IO` that
doesn't support monadic sequencing, but does support parallel processing. We don't want
users to have to think about `IO.Par`, though—what we want is for them to be able to indicate
which traversals or other operations in `IO` they want performed in parallel as opposed to sequentially.
As in our validation example, this is exactly the value added by `Parallel`: it gives us a richer
set of operations without forcing us to know or care about the underlying implementation.
