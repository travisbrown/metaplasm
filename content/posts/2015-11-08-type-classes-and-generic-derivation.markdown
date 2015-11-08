---
title: Type classes and generic derivation
date: Sun Nov  8 11:29:58 EST 2015
tags: scala, shapeless
---

Yesterday I wrote a [Stack Overflow answer][answer] about using Shapeless for generic derivation of
type class instances, and this morning I started putting together some
[new documentation][circe-derivation] for [circe][circe]'s generic derivation, and after a few
paragraphs I decided that it might make sense to write a blog post that could serve as a bridge
between the two—between simple examples like the one in my answer (which doesn't really go into
motivation, etc.) and the kinds of things we're doing in circe. I'll start more or less from
scratch, assuming only some basic familiarity with Scala syntax.

<!-- MORE -->

## The problem

The [Stack Overflow question][question] asks for a solution to a simple parsing problem: we've got
some case classes like this:

```scala
case class Person(name: String, age: Double)
case class Book(title: String, author: String, year: Int)
case class Country(name: String, population: Int, area: Double)
```

And we want to be able to create instances from a string representation that delimits fields with
commas:

```scala
val amy = Creator.create[Person]("Amy,54.2")
val fred = Creator.create[Person]("Fred,23")
val hamlet = Creator.create[Book]("Hamlet,Shakespeare,1600")
val finland = Creator.create[Country]("Finland,4500000,338424")
```

The question specifies only that the solution should not require a lot of boilerplate, but I'm going
to add two additional constraints. The first is that our API should never throw non-fatal
exceptions, where by "non-fatal" we mean exceptions that our program could in principle recover
from—e.g. if we see an `OutOfMemoryError`, we're perfectly justified in throwing it, but if we see a
`NumberFormatException` we need to handle it ourselves. This means that we'll have to handle the
possibility that input strings might not be valid representations of the type we're trying to parse
into—for example we can't construct a `Book` from `"23,Fred"`.

The second additional constraint is that our solution should keep track at compile time of what
types can be parsed. We should never get a runtime failure (either in the form of an exception or a
value representing failure) because we tried to parse a string into a type that we don't know
anything about.

## First try

A reasonable first draft of a solution might look like this:

```scala
trait RowParser[A] {
  def apply(s: String): A
}

val personParser: RowParser[Person] = new RowParser[Person] {
  def apply(s: String): Person = s.split(",").toList match {
    case List(name, age) => Person(name, age.toDouble)
  }
}

val bookParser: RowParser[Book] = new RowParser[Book] {
  def apply(s: String): Book = s.split(",").toList match {
    case List(title, author, year) => Book(title, author, year.toInt)
  }
}
```

And now we can write the following:

```scala
scala> personParser("Amy,54.2")
res0: Person = Person(Amy,54.2)

scala> bookParser("Hamlet,Shakespeare,1600")
res1: Book = Book(Hamlet,Shakespeare,1600)
```

The usage is pretty nice, thanks to Scala's syntactic sugar for methods named `apply`, and the fact
that our `RowParser` has a generic type parameter means that the results have useful types (they're
not just `Any` or whatever).

There are three problems with this solution, though. One problem is that it's easy to make our
parsers throw non-fatal exceptions:

```scala
scala> bookParser("Hamlet,Shakespeare")
scala.MatchError: List(Hamlet, Shakespeare) (of class scala.collection.immutable.$colon$colon)
  at $anon$1.apply(<console>:14)
  at $anon$1.apply(<console>:13)
  ... 33 elided
```

Another problem is just that while the usage is pretty clear, it doesn't support the kind of syntax
specified by the problem. We have to keep track of `RowParser` instances for each type that we want
to be able to parse into.

The last problem is closely related to the second: we're going to be writing a lot of boilerplate. We
have to define instances for every one of our case classes, and these definitions are extremely
repetitive.

## Failing more gracefully

The first problem is the easiest to fix—we can use Scala's `Option` type to represent the
possibility of failure, which allows us to return failures as values:

```scala
import scala.util.Try

trait SaferRowParser[A] {
  def apply(s: String): Option[A]
}

val personParser: SaferRowParser[Person] = new SaferRowParser[Person] {
  def apply(s: String): Option[Person] = s.split(",").toList match {
    case List(name, age) => Try(age.toDouble).map(Person(name, _)).toOption
    case _ => None
  }
}

val bookParser: SaferRowParser[Book] = new SaferRowParser[Book] {
  def apply(s: String): Option[Book] = s.split(",").toList match {
    case List(title, author, year) =>
      Try(year.toInt).map(Book(title, author, _)).toOption
    case _ => None
  }
}
```

And then:

```scala
scala> personParser("Amy,54.2")
res0: Option[Person] = Some(Person(Amy,54.2))

scala> bookParser("Hamlet,Shakespeare,1600")
res1: Option[Book] = Some(Book(Hamlet,Shakespeare,1600))

scala> bookParser("Hamlet,Shakespeare")
res2: Option[Book] = None
```

Now it's impossible to make our parsers crash with a recoverable exception—if the input is invalid
we just return a `None`.

## Runtime reflection

Next we'll try some different approaches to solving the syntax and boilerplate issues. If we were
writing Java, or Java-flavored Scala, we might consider using runtime reflection. Here for example
is a (lightly edited) solution from [one of the other answers][runtime] to yesterday's question:

```scala
import scala.reflect.ClassTag
import scala.util.Try

object ReflectiveRowParser {
  def apply[T: ClassTag](s: String): Option[T] = Try {
    val ctor = implicitly[ClassTag[T]].runtimeClass.getConstructors.head
    val paramsArray = s.split(",").map(_.trim)
    val paramsWithTypes = paramsArray.zip(ctor.getParameterTypes)

    val parameters = paramsWithTypes.map {
      case (param, cls) => cls.getName match {
        case "int" => param.toInt.asInstanceOf[Object]
        case "double" => param.toDouble.asInstanceOf[Object]
        case _ =>
          val paramConstructor = cls.getConstructor(param.getClass)
          paramConstructor.newInstance(param).asInstanceOf[Object]
      }
    }

    ctor.newInstance(parameters: _*).asInstanceOf[T]
  }.toOption
}
```

This works pretty nicely:

```scala
scala> ReflectiveRowParser[Person]("Amy,54.2")
res0: Option[Person] = Some(Person(Amy,54.2))

scala> ReflectiveRowParser[Person]("Fred,23")
res1: Option[Person] = Some(Person(Fred,23.0))

scala> ReflectiveRowParser[Book]("Hamlet,Shakespeare,1600")
res2: Option[Book] = Some(Book(Hamlet,Shakespeare,1600))

scala> ReflectiveRowParser[Country]("Finland,4500000,338424")
res3: Option[Country] = Some(Country(Finland,4500000,338424.0))

scala> ReflectiveRowParser[Book]("Hamlet,Shakespeare")
res4: Option[Book] = None
```

There's no boilerplate, and it won't throw recoverable exceptions. The only remaining problem is
that this approach knows absolutely nothing at compile time about what types it can or can't parse
into:

```scala
scala> ReflectiveRowParser[List[Book]]("Hamlet,Shakespeare,1600")
res5: Option[List[Book]] = None

scala> ReflectiveRowParser[Long]("Hamlet,Shakespeare,1600")
res6: Option[Long] = None

scala> trait Foo; case class Bar(a: String, b: String, c: Int) extends Foo
defined trait Foo
defined class Bar

scala> ReflectiveRowParser[Foo]("Hamlet,Shakespeare,1600")
res7: Option[Foo] = None
```

All of these operations fail at runtime. We could make this a little less annoying by using `Try` to
represent failure and providing error values that would tell us _why_ the operation failed (i.e.
either because we gave it bad input, or because we just don't know how to parse the required type),
but it's still annoying. If you've ever worked with a large Java project that uses Java libraries
that rely heavily on runtime reflection, you probably have some sense of _how_ annoying.

## Type classes

So let's go back to our `SaferRowParser`, which definitely didn't let us try to parse into types we
don't know how to parse into, and in fact made us keep track of our parsers for different types
individually. We want to adapt it so that it doesn't make us do this tracking ourselves, while also
not letting us try to parse any arbitrary type—i.e. we're aiming for something like this:

```scala
SaferRowParser[Book]("Hamlet,Shakespeare,1600") // compiles

trait Foo
SaferRowParser[Foo]("Hamlet,Shakespeare,1600")  // doesn't compile
```

As a first step we can give the type a companion object with an apply method:

```scala
object SaferRowParser {
  def apply[A](s: String)(parser: SaferRowParser[A]): Option[A] = parser(s)
}
```

Which lets us write this:

```scala
scala> SaferRowParser[Person]("Amy,54.2")(personParser)
res5: Option[Person] = Some(Person(Amy,54.2))

scala> SaferRowParser[Book]("Hamlet,Shakespeare,1600")(bookParser)
res6: Option[Book] = Some(Book(Hamlet,Shakespeare,1600))
```

This is more verbose than our original usage, but it also looks a little more like our target usage.

Now let's think about our `SaferRowParser` values. It's _possible_ to define multiple parsers for a
single type, but given that `SaferRowParser` is meant to parse a single format, it doesn't really
make sense—in fact we could imagine a kind of mapping from types to parsers.

Scala's implicit values provide one way of implementing such a mapping. We can demonstrate this by
making a couple of small changes to our companion object and `Person` and `Book` parser definitions:

```scala
import scala.util.Try

trait SaferRowParser[A] {
  def apply(s: String): Option[A]
}

object SaferRowParser {
  def apply[A](s: String)(implicit parser: SaferRowParser[A]): Option[A] = parser(s)

  implicit val personParser: SaferRowParser[Person] = new SaferRowParser[Person] {
    def apply(s: String): Option[Person] = s.split(",").toList match {
     case List(name, age) => Try(age.toDouble).map(Person(name, _)).toOption
      case _ => None
    }
  }

  implicit val bookParser: SaferRowParser[Book] = new SaferRowParser[Book] {
    def apply(s: String): Option[Book] = s.split(",").toList match {
      case List(title, author, year) =>
        Try(year.toInt).map(Book(title, author, _)).toOption
      case _ => None
    }
  }
}
```

We've just added the `implicit` keyword in a few places and have moved our parser definitions into
the companion object, and now we get the usage we want:

```scala
scala> SaferRowParser[Person]("Amy,54.2")
res1: Option[Person] = Some(Person(Amy,54.2))

scala> SaferRowParser[Book]("Hamlet,Shakespeare,1600")
res2: Option[Book] = Some(Book(Hamlet,Shakespeare,1600))

scala> trait Foo
defined trait Foo

scala> SaferRowParser[Foo]("Hamlet,Shakespeare,1600")
<console>:22: error: could not find implicit value for parameter parser: SaferRowParser[Foo]
       SaferRowParser[Foo]("Hamlet,Shakespeare,1600")
                          ^
```

We don't have to specify the parser instance, only the type we want to parse into, but if we try to
parse into an unknown type we get a compile-time error.

We've just invented [type classes][type-classes], which allow us to associate operations with types
without modifying the definitions of those types. Scala implements type classes through implicit
values and implicit arguments, but in other languages (like Haskell or Rust) type classes are
first-class language features.

We say that `SaferRowParser` is our type class, and that `SaferRowParser[Whatever]` is a _instance_
of `SaferRowParser` for `Whatever`. The `personParser` and `bookParser` definitions that provide our
type class instances here are both `val`s, but instances can also be provided by generic methods (as
we'll see in the next section).

## Generic derivation

So now we've got a solution that satisfies the two constraints we made up for ourselves (no
exceptions and compile-time tracking of what types we can parse), but it still requires a lot of
boilerplate. This is where generic derivation comes in.

We need to do something similar to what we saw in `ReflectiveRowParser`, but we want the reflection
to happen [at compile time, not runtime][reflection]. `ClassTag` isn't going to help us here—it will
allow us to get a list of a case class's members' types, but these are represented as a plain old
`List[Class]`, which gives us no useful type-level information.

Fortunately [Shapeless] provides a `Generic` type that we can think of as a kind of type-level
version of `ClassTag`. We can create a minimal [SBT][sbt] project that depends on Shapeless with a
`build.sbt` file like this:

```scala
scalaVersion := "2.11.7"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.2.5"
```

Now if we run `sbt console` we'll get a Scala REPL with Shapeless available. The rest of the
examples here will be run in this REPL, and will assume that you've defined the case classes from
the beginning of this post.

`Generic` (like `ClassTag`) is a type class, and we can look at the `Generic` instance for one of
our case classes like this:

```scala
scala> import shapeless._
import shapeless._

scala> Generic[Person]
res1: shapeless.Generic[Person]{type Repr = shapeless.::[String,shapeless.::[Double,shapeless.HNil]]} = fresh$macro$3$1@66aefec2
```

We can manually reformat the type for clarity (it's unfortunate that the REPL won't do this for us):

```scala
scala> Generic[Person]
res1: Generic[Person] {
  type Repr = String :: Double :: HNil
} = fresh$macro$3$1@66aefec2
```

The important thing to note here is that our `Generic` instance for `Person` includes type-level
information about the types of `Person`'s members. Instead of a `List[Class]`, we get an `HList`,
where the `H` is short for "heterogeneous" (or "heterogenous", which is the spelling [you'll find in
the Shapeless documentation][spelling]).

Heterogeneous lists are like tuples that are designed support abstraction over arity. Scala's tuples
are a set of 22 types that are unrelated apart from their names: `Tuple2` and `Tuple3`, for example,
don't extend some hypothetical `TupleN` (only a completely useless `Product` type), and there's no
generic way in the standard library to stick a type on the front of a `Tuple2` and get a `Tuple3`.

Shapeless's heterogeneous lists on the other hand share a useful super-type (`HList`), which allows
us to write generic methods and types that are polymorphic over the number of elements in the
`HList`, and Shapeless provides lots of operations for working with `HList`s, including
concatenation, filtering, etc.

So the `String :: Double :: HNil` type in our `Generic` instance is isomorphic to
`(String, Double)`—if we have a value of one type, we can convert it into the other, then convert
that result back to get our original value, etc. The `HList` representation is just easier to work
with than tuple types would be.

Now that we have a type-level version of the `ClassTag` we used in `ReflectiveRowParser`, the next
step is to build a parser from this information. In `ReflectiveRowParser` we just used `map` on our
`List[Class]` to figure out what type each field should be and to try to parse into that type. We
could do something similar to this with our `Repr` type, but instead we'll take a slightly different
approach that's more flexible and composable.

We'll start with our type class:

```scala
trait Parser[A] {
  def apply(s: String): Option[A]
}
```

Next we'll define some instances for basic types like `String` and `Int`:

```scala
import scala.util.Try

implicit val stringParser: Parser[String] = new Parser[String] {
  def apply(s: String): Option[String] = Some(s)
}

implicit val intParser: Parser[Int] = new Parser[Int] {
  def apply(s: String): Option[Int] = Try(s.toInt).toOption
}

implicit val doubleParser: Parser[Double] = new Parser[Double] {
  def apply(s: String): Option[Double] = Try(s.toDouble).toOption
}
```

Now for the fun part: we'll describe how to build up instances for `HList`s using induction:

```scala
import shapeless._

implicit val hnilParser: Parser[HNil] = new Parser[HNil] {
  def apply(s: String): Option[HNil] = if (s.isEmpty) Some(HNil) else None
}

implicit def hconsParser[H: Parser, T <: HList: Parser]: Parser[H :: T] = new Parser[H :: T] {
  def apply(s: String): Option[H :: T] = s.split(",").toList match {
    case cell +: rest => for {
      head <- implicitly[Parser[H]].apply(cell)
      tail <- implicitly[Parser[T]].apply(rest.mkString(","))
    } yield head :: tail
  }
}
```

This says that we know how to parse a string into an empty `HList` (i.e. `HNil`), and that we also
know how to parse a string into an `HList` made up of a head `H` and a tail `T`, but only if we know
how to parse into both `H` and `T`.

`hnilParser` is our base case and `hconsParser` is our inductive
step, and that's all we need to tell the compiler how to parse into any `HList` made up of things we
can parse into.

We need one more implicit method, though, since our case classes aren't literally `HList`s—they just
have `Generic` instances that map them to `HList`s. We need to provide `Parser` instances for types
that have `Generic` instances whose `Repr`s have `Parser` instances:

```scala
implicit def caseClassParser[A, R <: HList](implicit
  gen: Generic[A] { type Repr = R },
  reprParser: Parser[R]
): Parser[A] = new Parser[A] {
  def apply(s: String): Option[A] = reprParser.apply(s).map(gen.from)
}
```

In real code you're more likely to see `Generic.Aux[A, R]` than `Generic[A] { type Repr = R }`, but
I'm using this version (with the type refinement) because I think people often get too hung up on
the `Aux` stuff.

We can conclude by writing a companion object for `Parser` that'll support the syntax we want:

```scala
object Parser {
  def apply[A](s: String)(implicit parser: Parser[A]): Option[A] = parser(s)
}
```

(If you were using this in a real project, you'd want all of the `*Parser` methods above to go in
the companion object as well, but in the REPL it doesn't really matter.)

And finally we have a solution that does everything we wanted:

```scala
scala> Parser[Person]("Amy,54.2")
res1: Option[Person] = Some(Person(Amy,54.2))

scala> Parser[Person]("Fred,23")
res2: Option[Person] = Some(Person(Fred,23.0))

scala> Parser[Book]("Hamlet,Shakespeare,1600")
res3: Option[Book] = Some(Book(Hamlet,Shakespeare,1600))

scala> Parser[Country]("Finland,4500000,338424")
res4: Option[Country] = Some(Country(Finland,4500000,338424.0))

scala> Parser[Book]("Hamlet,Shakespeare")
res5: Option[Book] = None

scala> trait Foo
defined trait Foo

scala> Parser[Foo]("Hamlet,Shakespeare")
<console>:24: error: could not find implicit value for parameter parser: Parser[Foo]
       Parser[Foo]("Hamlet,Shakespeare")
                  ^
```

No exceptions, no boilerplate, no runtime reflection.

## Footnote

It's not entirely true that this last implementation meets all our requirements. For example:

```scala
scala> case class BookBook(b1: Book, b2: Book)
defined class BookBook

scala> Parser[BookBook]("Hamlet,Shakespeare")
res7: Option[BookBook] = None
```

Our format doesn't support any kind of nesting, so we don't actually know how to parse a string into
a `BookBook`, but our generic derivation mechanism is happy to give us an instance anyway. It's
still a pretty good solution, and it rules out many, many more cases of runtime failure than the
`ReflectiveRowParser` approach, but as it's written above the implementation isn't perfect.

It wouldn't be too hard to fix it (we'd just need to break our `Parser` type class into separate
`CellParser` and `RowParser` pieces), but the problem points to one of the really nice things about
this kind of generic derivation. When your format _does_ provide nesting, this approach to generic
derivation supports parsing it into nested case classes for free (in fact you have to do a little
extra work if you want to rule that out).

In [circe][circe]'s `generic` module, for example, we provide generic derivation for a `Decoder`
type class for decoding JSON, and in that case the support we get for decoding nested JSON objects
into nested case classes is exactly what we want.

The generic derivation in circe is much more advanced that what we've seen here (we can derive
decoders for sealed trait hierarchies, for example, and we need decoders that are aware of the names
of case class members, not just their types), but the fundamental ideas are exactly the same.

[answer]: http://stackoverflow.com/a/33586304/334519
[circe]: https://github.com/travisbrown/circe
[circe-derivation]: https://github.com/travisbrown/circe/issues/27
[question]: http://stackoverflow.com/q/33585441/334519
[reflection]: http://stackoverflow.com/a/33580411/334519
[runtime]: http://stackoverflow.com/a/33589126/334519
[ryos]: https://meta.plasm.us/posts/2015/07/11/roll-your-own-scala/
[sbt]: http://www.scala-sbt.org/
[shapeless]: https://github.com/milessabin/shapeless
[spelling]: https://github.com/milessabin/shapeless/issues/233
[type-classes]: https://en.wikipedia.org/wiki/Type_class
