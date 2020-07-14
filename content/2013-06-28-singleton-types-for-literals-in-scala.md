+++
title = "Singleton types for literals in Scala"
original_date = 2013-06-28T08:01:14
path = "posts/2013/06/28/singleton-types-for-literals-in-scala"

[taxonomies]
tags = ["scala", "macros"]
+++

It's sometimes useful in Scala to have a type with a single value.
These are called _singleton types_, and they show up most easily in the
context of Scala's objects. For example, if we have the following definition:

``` scala
object foo {
  def whatever = 13
}
```

We can refer to a type `foo.type` that is the singleton type for `foo`—i.e.,
the type that contains nothing except `foo`. We can use this type to write a function that
won't compile with any non-`foo` argument:

``` scala
def fooIdentity(x: foo.type) = x
```

For example:

``` scala
scala> fooIdentity(foo)
res1: foo.type = foo$@5da19724

scala> fooIdentity("foo")
<console>:14: error: type mismatch;
 found   : String("foo")
 required: foo.type
              fooIdentity("foo")
                          ^
```

Note that this error message doesn't just tell us that we provided a
`String` when we needed a `foo`—it lists the type as `String("foo")`. This
is because string literals—like all other literals in Scala
(except function literals)—are also singletons in
the sense that their most specific type is a singleton type.

<!-- more -->

Unfortunately Scala doesn't provide the `.type` syntax for
literals—we can't write `"foo".type`, for example (except in a 
[compiler fork](https://github.com/paulp/scala/commit/824ce5e) by [Paul Phillips](https://twitter.com/extempore2)).
[Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html)'s [type macros](http://docs.scala-lang.org/overviews/macros/typemacros.html)
get pretty close by allowing us to write something like `singleton("foo")` to refer to this
type, but the 2.11 release is half a year away, and there's no guarantee it will
include type macros, anyway. We can get what we want while we're waiting, though, if we're willing
to pay a bit of syntax tax.

We can start with the following macro:

``` scala
import scala.language.experimental.macros
import scala.reflect.macros.Context

class LiteralSingleton[In <: Singleton] { type T = In }

object LiteralSingleton {
  def apply_impl[A: c.WeakTypeTag](c: Context)(a: c.Expr[A]) = {
    import c.universe._

    def `new`(t: Type) = Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), Nil)

    a.tree match {
      case Literal(const: Constant) => c.Expr[LiteralSingleton[_]](
        `new`(
          appliedType(
            typeOf[LiteralSingleton[_]].typeConstructor,
            ConstantType(const) :: Nil
          )
        ) 
      )
      case _ => c.abort(c.enclosingPosition, "Not a literal!")
    }
  }
}
```

This isn't paradise, but it is code you can copy and paste into your 2.10 REPL today.
Next we can define a method for lifting values into `LiteralSingleton`:

``` scala
def ^[A](a: A) = macro LiteralSingleton.apply_impl[A]
```

And then:

``` scala
scala> val thirteen = ^(13)
thirteen: LiteralSingleton[Int(13)] = LiteralSingleton@6ae26e57

scala> type _13 = thirteen.T
defined type alias _13

scala> def only13(x: _13): Int = x
only13: (x: _13)Int

scala> only13(14)
<console>:16: error: type mismatch;
 found   : Int(14)
 required: _13
    (which expands to)  Int(13)
              only13(14)
                     ^

scala> only13(13)
res0: Int = 13
```

The type alias is just a syntactic convenience—we could just as easily
refer to `thirteen.T` directly in our method definitions, etc.

We can also write a macro that will give us the (unique) inhabitant of
a singleton type:

``` scala
case class Inhabitant[A <: Singleton](a: A)

object Inhabitant { 
  implicit def witness[A <: Singleton] = macro witness_impl[A]

  def witness_impl[A <: Singleton: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val value = c.weakTypeOf[A].normalize match {
      case ConstantType(const) => Literal(const)
      case SingleType(pre, sym) => Select(TypeTree(pre), sym)
      case _ => c.abort(c.enclosingPosition, "Not a singleton type!")
    }

    c.Expr[Inhabitant[A]](
      Apply(
        TypeApply(
          Select(reify(Inhabitant).tree, newTermName("apply")),
          TypeTree(c.weakTypeOf[A]) :: Nil
        ),
        value :: Nil
      )
    )
  }
}
```

And then:

``` scala
def inhabitant[A <: Singleton](implicit h: Inhabitant[A]) = h.a
```

Now we can write the following:

``` scala
scala> inhabitant[foo.type].whatever
res1: Int = 13

scala> val w = ^("whatever")
w: LiteralSingleton[String("whatever")] = LiteralSingleton@5992f82e

scala> inhabitant[w.T]: String
res2: String = whatever

scala> inhabitant[_13]: Int
res3: Int = 13
```

For a concrete example of how singleton types for literals can be used in Scala, see
[Alois Cochard](https://twitter.com/aloiscochard)'s [Shona library](https://github.com/aloiscochard/shona)
and [this ScalaDays 2013 presentation](http://parleys.com/play/51c0d0ece4b0ed877035680e/chapter0/about) by
[Ismael Juma](https://twitter.com/ijuma) and Alois.
Shona uses type macros instead of the approach I've described here,
but I've put together [a proof-of-concept port](https://github.com/travisbrown/shona) to 2.10 that only needs `def` macros.

