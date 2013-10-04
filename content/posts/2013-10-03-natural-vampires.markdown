---
title: Natural vampires
date: Thu Oct  3 21:31:38 EDT 2013
tags: scala, macros
---

[This Stack Overflow question](http://stackoverflow.com/q/19170137/334519) is interesting—it asks whether we can use
Scala macros to create a value class for positive integers where the positiveness is checked at compile-time,
and where it's not possible to create an invalid instance.

I'm pretty sure it's not. My first thought was to turn `PosInt` into a sealed [universal trait](http://docs.scala-lang.org/overviews/core/value-classes.html)
with a private value class implementation in the `PosInt` companion object,
but inheriting from a universal trait forces us to give up most (all?) of the advantages of value classes in
this case, and of course it's not actually possible to make the value class private,
anyway.

So I don't have an answer, but I do have a pretty neat trick involving [vampire methods](http://meta.plasm.us/posts/2013/07/12/vampire-methods-for-structural-types/)
that gives us some of the benefits of value classes.

<!-- MORE -->

If you've seen vampire methods before, the code will look pretty familiar:

``` scala
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.Context

class body(tree: Any) extends StaticAnnotation

object PosInt {
  def apply(i: Int) = macro apply_impl

  def apply_impl(c: Context)(i: c.Expr[Int]): c.Expr[Any] = {
    import c.universe._

    i.tree match {
      case Literal(Constant(n: Int)) if n > 0 => c.Expr(
        q"""
          class Pos {
            @body($n) def value: Int = macro PosInt.selectValue_impl
          } 
          new Pos {}
        """
      )
      case Literal(Constant(n: Int)) => c.abort(
        c.enclosingPosition,
        "%d is not a positive integer!".format(n)
      )
      case _ => c.abort(c.enclosingPosition, "Not a literal!")
    }
  }

  def selectValue_impl(c: Context) = c.Expr(
    c.macroApplication.symbol.annotations.filter(
      _.tpe <:< c.typeOf[body]
    ).head.scalaArgs.head
  )
}
```

Now it's impossible to use the `PosInt` constructor with a negative integer:

``` scala
scala> val unthirteen = PosInt(-13)
<console>:28: error: -13 is not a positive integer!
       val unthirteen = PosInt(-13)
                              ^
```

When we give it a positive integer, we get a structurally-typed value:

``` scala
scala> val thirteen = PosInt(13)
thirteen: AnyRef{def value: Int} = $anon$1@1814ac5c
```

This looks like bad news, but the neat part is that when we write something like this:

``` scala
scala> thirteen.value.toString + "!!!"
res1: String = 13!!!
```

The expression is actually being rewritten _at compile-time_ to `13.toString + "!!!"`.
No reflective access or even method calls of any kind—just a nice clean constant
in our code after macro expansion.

Again: it's definitely not an answer to the question, and it's nowhere near as nice as
a real value class would be—there's still an object allocation, for example—but it's an
example of how plain old ([whitebox](http://docs.scala-lang.org/overviews/macros/blackbox-whitebox.html)) `def` macros
let us do something crazy without special language support.

