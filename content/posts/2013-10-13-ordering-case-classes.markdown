---
title: Ordering case classes
date: Sun Oct 13 16:23:12 EDT 2013
tags: scala, shapeless, macros
---

Scala provides lexicographic `Ordering` instances for
`TupleN` types when all of the element types have `Ordering` instances.
It doesn't provide the same instances for case classes, however—probably
just because lexicographic order isn't what you want for case classes as
often as it is for tuples.

Sometimes [you actually do want a lexicographic ordering](https://stackoverflow.com/q/19345030/334519)
for your case classes, though, and Scala unfortunately doesn't provide any nice
boilerplate-free way to create such orderings. This post will provide a
quick sketch of two approaches to filling this gap: one using macros, and
one using [Shapeless 2.0](https://github.com/milessabin/shapeless)'s new `Generic` machinery
and the `TypeClass` type class.

First for a case class to use as a running example, along with some instances:

``` scala
case class Foo(x: Int, y: String)

val a = Foo(9, "x")
val b = Foo(1, "z")
val c = Foo(9, "w")

val foos = List(a, b, c)
```

Let's quickly confirm that there's no `Ordering[Foo]` already sitting around:

``` scala
scala> foos.sorted
<console>:14: error: No implicit Ordering defined for Foo.
              foos.sorted
                   ^
```

Yep, we're going to have to take care of this ourselves.

<!-- MORE -->

First for the macro solution (note that I'm using [quasiquotes](https://meta.plasm.us/posts/2013/09/06/quasiquotes-for-multiple-parameter-lists/),
which are available in Scala 2.10 as [a plugin](http://docs.scala-lang.org/overviews/macros/paradise.html)):

``` scala
import scala.language.experimental.macros
import scala.reflect.macros.Context

object OrderingHelper {
  implicit def apply[A]: Ordering[A] = macro apply_impl[A]

  def apply_impl[A: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val fields = weakTypeOf[A].declarations.collect {
      case sym: MethodSymbol if sym.isCaseAccessor => q"a.${sym.name}"
    }

    if (fields.isEmpty) {
      c.abort(c.enclosingPosition, "Not a case class!")
    } else {
      val scalaSym = typeOf[Any].typeSymbol.owner

      weakTypeOf[A].baseClasses.collectFirst {
        case sym
          if sym.name.decoded.startsWith("Tuple") && sym.owner == scalaSym =>
            c.abort(c.enclosingPosition, "Not needed for tuples!")
      } getOrElse c.Expr[Ordering[A]](q"Ordering.by(a => (..$fields))")
    }
  }
}
```

Apart from the fact that we need to confirm that we don't have a tuple (which
would throw us into an infinitely recursive implicit search and crash the compiler),
this is pretty straightforward stuff—we just collect all the case accessors
and plop them into a tuple, which we know how to order lexicographically.

One import, and we're done:

``` scala
scala> import OrderingHelper._
import OrderingHelper._

scala> foos.sorted
res0: List[Foo] = List(Foo(1,z), Foo(9,w), Foo(9,x))
```

This is all fairly nice and type-safe, in the sense that if our case class
has an unordered member, we get a reasonably helpful compile-time error:

``` scala
scala> trait Unsortable
defined trait Unsortable

scala> case class Bar(u: Unsortable)
defined class Bar

scala> OrderingHelper[Bar]
<console>:20: error: No implicit Ordering defined for (Unsortable,).
              OrderingHelper[Bar]
                            ^
```

And even that could be pretty easily improved. It's still arguably kind
of clunky and ad-hoc, though, which is [where Shapeless comes in](https://twitter.com/milessabin/status/389444004393541632). With
[Lars Hupel](https://twitter.com/larsr_h)'s `ProductTypeClass` type class, we
can just describe how to build up lexicographic ordering instances:

``` scala
import shapeless._

implicit object `Ordering's a type class` extends ProductTypeClass[Ordering] {
  def product[H, T <: HList](ho: Ordering[H], to: Ordering[T]) =
    Ordering.Tuple2(ho, to).on(l => (l.head, l.tail))

  def emptyProduct = Ordering.by(_ => ())

  def project[F, G](instance: => Ordering[G], to: F => G, from: G => F) =
    instance on to
}
```

And `Generic` will take care of (almost all of) the rest.

``` scala
scala> object OrderingHelper extends TypeClassCompanion[Ordering]
defined module OrderingHelper

scala> import OrderingHelper.auto._
import OrderingHelper.auto._

scala> foos.sorted
res0: List[Foo] = List(Foo(1,z), Foo(9,w), Foo(9,x))
```

Much nicer!

