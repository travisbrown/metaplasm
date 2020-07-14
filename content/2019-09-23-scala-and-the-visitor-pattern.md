+++
title = "Scala and the visitor pattern"
original_date = 2019-09-23T18:26:25
path = "posts/2019/09/23/scala-and-the-visitor-pattern"

[taxonomies]
tags = ["scala", "fp", "circe", "json"]
+++

Scala provides a handful of language features that are designed to make it easy
for users to define and work with 
[algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type) (ADTs).
You write some case classes that extend a sealed trait, you write some functions
that pattern match on those case classes,
and you're done: you have a nice linked list or rose tree or whatever.

Sometimes you can't use case classes to implement your variants, though,
or you don't want to put your case classes in your public API, and in these
situations pattern matching is typically much less useful. This blog post is about
the [visitor pattern](https://en.wikipedia.org/wiki/Visitor_pattern), which
is an alternative to pattern matching that provides many of its benefits, and about
the use of visitors we're planning for Circe 1.0.

<!-- more -->

## Pattern matching

To start with a relatively simple example (no generics, only two variants), the following would be a reasonable
implementation of an immutable linked list of integers in Scala:

```scala
sealed trait IntList

case object IntNil extends IntList
case class IntCons(h: Int, t: IntList) extends IntList
```

The sealed trait here is our sum type, and the case object and class are our product (or variant) types.
We can use Scala's pattern matching to write operations on this ADT:

```scala
def sum(xs: IntList): Int = xs match {
  case IntNil => 0
  case IntCons(h, t) => h + sum(t)
}
```
One of the nice things about pattern matching on sum types in Scala is that the compiler checks
that our cases are exhaustive. If we forget one, the compiler will warn us:

```scala
scala> def sum(xs: IntList): Int = xs match {
     |   case IntCons(h, t) => h + sum(t)
     | }
                                   ^
       warning: match may not be exhaustive.
       It would fail on the following input: IntNil
sum: (xs: IntList)Int
```

We can use the same approach to model more complex data types, like an abstract syntax tree representing JSON documents:

```scala
sealed trait Json

case object JsonNull extends Json
case class JsonBoolean(value: Boolean) extends Json
case class JsonNumber(value: BigDecimal) extends Json
case class JsonString(value: String) extends Json
case class JsonArray(value: Vector[Json]) extends Json
case class JsonObject(value: Vector[(String, Json)]) extends Json
```

We can implement operations on this ADT using pattern matching, just like we did for `sum` above:

```scala
def countValues(json: Json): Int = json match {
  case JsonNull       => 0
  case JsonBoolean(_) => 1
  case JsonNumber(_)  => 1
  case JsonString(_)  => 1
  case JsonArray(js)  => js.map(countValues).sum
  case JsonObject(fs) => fs.map(field => countValues(field._2)).sum
}
```

And again if we forget one of these cases, the compiler will tell us.

## Optimizing our representation

Unfortunately if we care about performance we'll quickly run into some problems with
this representation. We might want to add a number variant for integers that fit
in a `Long`, for example, since `BigDecimal` is an expensive way to represent these:

```scala
case class JsonLong(value: Long) extends Json
```

Now we're faced with a design decision. We could expose this variant to our users,
or we could make it private. The fact that we have two ways of representing JSON
numbers feels like an implementation detail that our users probably shouldn't have to worry
about, so there's an argument for making these case classes private.

In reality these decisions are often even easier. In [Circe](https://github.com/circe/circe),
for example, we currently have two JSON object representations, which would translate into our simplified
example like this:

```scala
import java.util.LinkedHashMap

case class LHMJsonObject(value: LinkedHashMap[String, Json]) extends Json
case class MVJsonObject(keys: Vector[String], value: Map[String, Json]) extends Json
```
We can significantly improve performance by building up a mutable linked map during parsing,
and looking up values in a `LinkedHashMap` is also measurably faster than Scala's immutable
`Map`. We want our `Json` values to be immutable, though, so we definitely can't give our users
access to the underlying representation in that case.

## Extractors

To return to our number optimization, even if we make the number case classes private, we can
still support pattern matching, thanks to Scala's [extractors](https://docs.scala-lang.org/tour/extractor-objects.html):

```scala
sealed trait Json

case object JsonNull extends Json
case class JsonBoolean(value: Boolean) extends Json
private case class JsonBigDecimalNumber(value: BigDecimal) extends Json
private case class JsonLongNumber(value: Long) extends Json
case class JsonString(value: String) extends Json
case class JsonArray(value: Vector[Json]) extends Json
case class JsonObject(value: Vector[(String, Json)]) extends Json

object JsonNumber {
  def unapply(json: Json): Option[BigDecimal] = json match {
    case JsonBigDecimalNumber(value) => Some(value)
    case JsonLongNumber(value) => Some(BigDecimal(value))
    case _ => None
  }
}
```
Even though we no longer have a `JsonNumber` case class, Scala
allows us to use `JsonNumber` as a case in pattern matching because we've
defined an appropriately-typed `unapply` method in the `JsonNumber` object.
This means we can write our `countValues`
method in exactly the same way as we did before, when we actually did have
a `JsonNumber` case class.

The problem with this approach is that as soon as we start using custom extractors,
we lose exhaustivity checking, which in my view makes it a non-starter. Pattern matching syntax
is nice, but ending up with runtime errors because you forgot a case and the compiler didn't tell you is
not.

It's worth noting that Circe does provide extractors for `Json`, in a separate [circe-optics](https://github.com/circe/circe-optics)
module:

```scala
import io.circe.Json, io.circe.optics.all._

def countValues(json: Json): Int = json match {
  case jsonBoolean(_) => 1
  case jsonNumber(_)  => 1
  case jsonString(_)  => 1
  case jsonArray(js)  => js.map(countValues).sum
  case jsonObject(fs) => fs.values.map(countValues).sum
  case _              => 0
}
```

These extractors are all [prisms](http://julien-truffaut.github.io/Monocle/optics/prism.html) from
[Monocle](http://julien-truffaut.github.io/Monocle/), a Scala lens library, and while they still
don't provide exhaustivity checking in pattern matching, they do have many other useful features
(which are unfortunately outside the scope of this blog post).

## "Folding"

If you've ever used [Argonaut](http://argonaut.io) or Circe, you might know that while they don't support pattern matching on their JSON AST case classes,
they do both provide a `fold` method as an alternative to pattern matching:

```scala
import io.circe.Json

def countValues(json: Json): Int = json.fold(
  0,
  _ => 1,
  _ => 1,
  _ => 1,
  js => js.map(countValues).sum,
  fs => fs.values.map(countValues).sum
)
```

We can make the resemblance to pattern matching clearer by using named arguments:

```scala
def countValues(json: Json): Int = json.fold(
  jsonNull    =      0,
  jsonBoolean = _  => 1,
  jsonNumber  = _  => 1,
  jsonString  = _  => 1,
  jsonArray   = js => js.map(countValues).sum,
  jsonObject  = fs => fs.values.map(countValues).sum
)
```

Both versions are actually a little more concise than our pattern-matching implementation, although `fold` is less flexible in some
ways (we can't use guards, etc.), and the version with positional arguments in particular is arguably less readable. But `fold` still gives us the moral equivalent of exhaustivity,
in the sense that the compiler won't allow us to skip a case, while also leaving us with a lot of freedom to optimize
our JSON representation without forcing details on our users.

## Folders

One disadvantage of the `fold` approach is that every call requires us to provide six functions, and given
idiomatic pattern matching-like usage, that means instantiating six objects. This can have a real impact on performance,
and Circe provides an alternative alternative for performance-sensitive users:

```scala
import io.circe.{Json, JsonNumber, JsonObject}
import io.circe.Json.Folder

val countValues: Folder[Int] = new Folder[Int] {
  def onNull: Int = 0
  def onBoolean(value: Boolean): Int = 1
  def onNumber(value: JsonNumber): Int = 1
  def onString(value: String): Int = 1
  def onArray(value: Vector[Json]): Int = value.map(_.foldWith(this)).sum
  def onObject(value: JsonObject): Int = value.values.map(_.foldWith(this)).sum
}
```
…which we can use like this:

```scala
scala> import io.circe.literal._
import io.circe.literal._

scala> json"[1,2,true,[4,5,6],null,[[[7]]]]".foldWith(countValues)
res0: Int = 7
```

All this `Folder` type does is allow us to encapsulate six functions in a single object.
We have [some benchmarks](https://github.com/circe/circe/blob/7285659dd415bf686b208f8a4bc47e038ce4af02/modules/benchmark/src/main/scala/io/circe/benchmark/FoldingBenchmark.scala)
in Circe that try to model realistic usage,
and they show `foldWith` getting over 70% more throughput than `fold`. In fact `foldWith` also outperforms
pattern matching in those benchmarks (shown here for Scala 2.13):

```
Benchmark                           Mode  Cnt      Score    Error  Units
FoldingBenchmark.withFold          thrpt   20   6491.030 ± 13.383  ops/s
FoldingBenchmark.withFoldWith      thrpt   20  11353.992 ± 98.429  ops/s
FoldingBenchmark.withPatternMatch  thrpt   20   8307.922 ± 27.285  ops/s
```
For most users these differences are likely to be irrelevant, and `fold` will work just fine.
Personally I've come to prefer `Folder` even when I'm not terribly worried about minimizing allocations, though. 

## Visitors

If you've read your [Gang of Four](https://en.wikipedia.org/wiki/Design_Patterns) (which turns 25 this year),
you might recognize `Folder` as an instance of the [visitor pattern](https://en.wikipedia.org/wiki/Visitor_pattern). This isn't too surprising, since the visitor pattern is an attempt to solve the same kinds of problems that ADTs solve,
usually in languages that don't have pattern matching or higher-order functions, and what we're doing is trying to come up with a nice way to work
with ADTs without pattern matching (because we want implementation hiding but also exhaustivity checking) or higher-order functions (for performance reasons).

The original Gang of Four framing of the visitor pattern is pretty strongly imperative-flavored, but there's [a 2005 paper](http://www.computer-science.birmingham.ac.uk/~hxt/research/mfps-visitors.pdf)
by Peter Buchlovsky and Hayo Thielecke that gives a type-theoretic view of the pattern and looks in detail at its relationship with algebraic data types.

For me the most useful part of the paper is the distinction it introduces between "internal" and "external" visitors:

> Another aspect of the Visitor pattern is the choice of traversal strategies for composite objects. We could put the traversal code in the datatype. To do this we ensure that the accept method is called recursively on any component objects and passes the results to the visitor in the call to visit. Alternatively, we could put the traversal code in the visitor itself. We will refer to these as “internal” and “external” visitors respectively, by analogy with internal and external iterators.

Using this terminology, the `fold` and `foldWith` in Argonaut and Circe are examples of external visitors, since they require the user to recurse explicitly in the cases of arrays and objects. The external visitor for our `IntList` ADT would look like this:

```scala
sealed trait IntList {
  def accept[A](visitor: (A, (Int, IntList) => A)): A
}

case object IntNil extends IntList {
  def accept[A](visitor: (A, (Int, IntList) => A)): A = visitor._1
}

case class IntCons(h: Int, t: IntList) extends IntList {
  def accept[A](visitor: (A, (Int, IntList) => A)): A = visitor._2(h, t)
}
```
Which we could use like this:

```scala
def sum(xs: IntList): Int = xs.accept[Int]((0, _ + sum(_)))
```

Or, to emphasize the resemblance to our original pattern-matching implementation:

```scala
def sum(xs: IntList): Int = xs.accept[Int]((
  0,
  (h, t) => h + sum(t)
))
```

The internal visitor for `IntList` would be something like the following. Note that the recursion happens inside the `accept` implementation, not in the visitor itself:

```scala
sealed trait IntList {
  def accept[A](visitor: (A, (Int, A) => A)): A
}

case object IntNil extends IntList {
  def accept[A](visitor: (A, (Int, A) => A)): A = visitor._1
}

case class IntCons(h: Int, t: IntList) extends IntList {
  def accept[A](visitor: (A, (Int, A) => A)): A =
    visitor._2(h, t.accept(visitor))
}
```

We can now rewrite our `sum` method like this:

```scala
def sum(xs: IntList): Int = xs.accept[Int]((0, _ + _))
```

This might look familiar, because the standard library's `List` has a method that's almost identical:

```scala
def sum(xs: List[Int]): Int = xs.fold[Int](0)(_ + _)
```

Once again this resemblance isn't an accident: internal visitors are "basically folds", as [another paper](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/visitor.pdf) puts it.

To summarize: external visitors are like pattern matching, giving the user access to one layer of structure at a time,
and allowing them to recurse into the next layer as needed, while internal visitors are like folds, where the data structure
itself drives the recursion.

## Fixing Circe

For years I've been uncomfortable about the fact that Argonaut (and later Circe) has a `fold` method that doesn't really seem like a fold. The distinction between internal and external visitors helps to make the problem clear: folds are internal visitors, while what Argonaut and Circe currently provide are external visitors.

The distinction also helps to highlight a gap in the API. For operations like our `countValues` method above, it would be much nicer if we didn't have to recurse manually. With the internal visitor approach, we could write the following:

```scala
def countValues(json: Json): Int = json.acceptInternalVisitor(
  0,
  _ => 1,
  _ => 1,
  _ => 1,
  _.sum,
  _.foldMap(_._2)
)
```

While the external visitor approach is more flexible, and handles more use cases, also providing internal visitors would give many common operations (like computing statistics about a document) more straightforward implementations.

One additional advantage of the internal visitor approach is that the `accept` implementation can take responsibility for stack safety (see [this gist](https://gist.github.com/travisbrown/4ec4047f483d8dbeb1672d0a36e352a5), for example), so that no matter how deeply nested our JSON document is, we don't have to worry about recursion overflowing the stack. It's certainly possible to recurse safely with `Folder` right now, but you have to do all the trampolining manually.

It's likely that Circe 1.0 will rename the current `Folder` to `Visitor`, as our external visitor, and will introduce a new stack-safe internal visitor as `Folder`. I'm also working on an `io.circe.internal` package that will provide an external visitor API that _will_ expose implementation details (such as the number representation, the specific map implementation, etc.). We're currently finalizing names and other details for these types and methods in Circe 1.0, and would appreciate any [ideas or feedback](https://github.com/circe/circe/issues/1097).


