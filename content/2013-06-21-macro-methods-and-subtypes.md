+++
title = "Macro methods and subtypes"
original_date = 2013-06-21T08:43:15
path = "posts/2013/06/21/macro-methods-and-subtypes"

[taxonomies]
tags = ["scala", "macros"]
+++

Suppose we want to define an `HListable` trait in Scala that will add a
`members` method returning an `HList` of member values to any case
class that extends it. This would let us write the following, for
example:

``` scala
scala> case class User(first: String, last: String, age: Int) extends HListable
defined class User

scala> val foo = User("Foo", "McBar", 25)
foo: User = User(Foo,McBar,25)

scala> foo.members == "Foo" :: "McBar" :: 25 :: HNil
res0: Boolean = true
```

So we try the following, which looks reasonable at a glance:

<!-- more -->

``` scala
import scala.language.experimental.macros
import shapeless._

trait HListable {
  def members: HList = macro HListable.members_impl[this.type]
}

object HListable {
  import scala.reflect.macros.Context

  def members_impl[A <: HListable: c.WeakTypeTag](c: Context): c.Expr[HList] = {
    import c.universe._

    weakTypeOf[A].declarations.collect {
      case m: MethodSymbol if m.isCaseAccessor => m
    }.foldRight(reify(HNil: HList)) {
      case (m, acc) =>
        val value = c.Expr(Select(c.resetAllAttrs(c.prefix.tree), m.name))
        reify(value.splice :: acc.splice)
    }
  }
}
```

Unfortunately it doesn't actually work:

``` scala
scala> foo.members
res0: shapeless.HList = HNil
```

The problem is that the `this` in the type argument to the macro is resolved
too early—we don't get the kind of late binding we generally expect in Scala.
I'm using type-level lists in this example, but the problem we're running
into here is much more general, and
can turn up any time you're writing a macro method that needs information
about the type of a subtype of the class or trait it's defined in.

One solution would be to use _F_-bounded polymorphism:

``` scala
trait HListable[A <: HListable[A]] {
  def members: HList = macro HListable.members_impl[A]
}
```

This works...

``` scala
scala> case class User(first: String, last: String, age: Int) extends HListable[User]
defined class User

scala> val foo = User("Foo", "McBar", 25)
foo: User = User(Foo,McBar,25)

scala> foo.members == "Foo" :: "McBar" :: 25 :: HNil
res0: Boolean = true
```

But ugh, that's a lot of complexity to add for this one little bit of functionality.

Another solution (seen in
[this Stack Overflow question](https://stackoverflow.com/q/17223213/334519),
for example) is to add a type parameter to the method:

``` scala
trait HListable {
  def members[A <: HListable]: HList = macro HListable.members_impl[A]
}
```

This also works...

``` scala
scala> foo.members[User] == "Foo" :: "McBar" :: 25 :: HNil
res0: Boolean = true
```

But it's annoying that we have to indicate that we're talking about a `User` when the
compiler already knows that that's exactly what `foo` is.

We can get exactly what we want, though, through the magic of implicit classes:

``` scala
import scala.language.experimental.macros
import shapeless._

trait HListable

object HListable {
  import scala.reflect.macros.Context

  implicit class HListThisThing[A <: HListable](val a: A) extends AnyVal {
    def members: HList = macro HListable.members_impl[A]
  }

  def members_impl[A <: HListable: c.WeakTypeTag](c: Context): c.Expr[HList] = {
    import c.universe._

    weakTypeOf[A].declarations.collect {
      case m: MethodSymbol if m.isCaseAccessor => m
    }.foldRight(reify(HNil: HList)) {
      case (m, acc) =>
        val value = c.Expr(
          Select(
            Select(c.resetAllAttrs(c.prefix.tree), newTermName("a")),
            m.name
          )
        )
        reify(value.splice :: acc.splice)
    }
  }
}
```

And now:

``` scala
scala> foo.members == "Foo" :: "McBar" :: 25 :: HNil
res0: Boolean = true
```

See [this Stack Overflow answer](https://stackoverflow.com/a/17224392/334519)
for another example of this trick in action.

