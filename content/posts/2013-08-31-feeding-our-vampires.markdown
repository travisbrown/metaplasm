---
title: Feeding our vampires
date: Sat Aug 31 12:31:16 EDT 2013
tags: scala, macros
---

I've written [several](http://meta.plasm.us/posts/2013/07/12/vampire-methods-for-structural-types/)
[times](http://stackoverflow.com/a/18485004/334519)
about _vampire methods_, which are macro
methods inside a macro-defined type whose implementations are provided in
an annotation. Normally when we define a type in a `def` macro, it looks like
a structural type to the outside world, and calling methods on a structural
type involves reflective access. Vampire methods allow us to avoid that ugly
bit of runtime reflection.

This trick (which was [first discovered](https://twitter.com/xeno_by/status/355003437844398083)
by [Eugene Burmako](https://twitter.com/xeno_by)) is useful because it makes
it a little more practical to use `def` macros to approximate
[type providers](http://meta.plasm.us/posts/2013/07/11/fake-type-providers-part-2/),
for example.
It's also just really clever.

For methods with no parameters, the execution of the trick is pretty straightforward.
It's a little more tricky when we do have parameters,
as [Eric Torreborre](http://etorreborre.blogspot.com/)
notes in a question [here](http://stackoverflow.com/q/18523871/334519), since in that
case the annotation will need to contain a function instead of just a simple
constant of some kind.

Let's take a first quick stab at an example:

``` scala
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.Context

class body(tree: Any) extends StaticAnnotation

object VampireExample {
  def demo(name: String) = macro demo_impl

  def demo_impl(c: Context)(name: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    val methodName = name.tree match {
      case Literal(Constant(s: String)) => newTermName(s)
      case _ => c.abort(c.enclosingPosition, "Must provide a literal name!")
    }

    val className = newTypeName(c.fresh())

    c.Expr[Any](
      q"""
        class $className {
          @body((x: Int) => x + 1)
          def $methodName(x: Int): Int = macro VampireExample.method_impl
        }
        new $className {}
      """
    )
  }

  def method_impl(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._

    val (arg, body) = c.macroApplication.symbol.annotations.filter(
      _.tpe <:< typeOf[body]
    ).headOption.flatMap(
      _.scalaArgs.collectFirst {
        case Function(ValDef(_, arg, _, _) :: Nil, body) =>
          arg -> c.resetAllAttrs(body)
      }
    ).getOrElse(
      c.abort(c.enclosingPosition, "Annotation body not provided!")
    )

    c.Expr(q"val $arg = $x; $body")
}
```

Here `VampireExample.demo` takes a name for a method and returns an instance
of a structural type with an integer increment method with that name.

``` scala
scala> val fooer = VampireExample.demo("foo")
fooer: AnyRef{def foo(x: Int): Int} = $anon$1@28c014d5

scala> fooer.foo(13)
res0: Int = 14
```

Note that we have to import `scala.language.reflectiveCalls` if we don't
want to see warnings about reflective access here. This is because the compiler
is being stupid—there's not actually any reflective access happening, as you
can confirm for yourself if you're working in a REPL with `-Xprint:cleanup`.

We have a problem, though. Suppose we want to refer to some other methods
in our macro-generated type in our annotation-borne implementation function.
In that case we'll end up with lost `this` references in the tree build
by `method_impl` (you can see Eric's
[Stack Overflow question](http://stackoverflow.com/q/18523871/334519) for the details).

Unfortunately I don't think there's any way to use the `substituteThis` method
on `Tree`, since I'm not sure how we'd get the symbol for our class at that point.
But we can manually replace the references to `this` with the macro's prefix using
a tree transformer:

```
object VampireExample {
  def demo(name: String) = macro demo_impl

  def demo_impl(c: Context)(name: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    val methodName = name.tree match {
      case Literal(Constant(s: String)) => newTermName(s)
      case _ => c.abort(c.enclosingPosition, "Must provide a literal name!")
    }

    val className = newTypeName(c.fresh("COVEN"))

    c.Expr[Any](
      q"""
        class $className {
          val baz = 10
          @body((x: Int) => x + baz)
          def $methodName(x: Int): Int = macro VampireExample.method_impl
        }
        new $className {}
      """
    )
  }

  def method_impl(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._

    val prefixVal = newTermName(c.fresh())

    object replaceThises extends Transformer {
      override def transform(tree: Tree) = tree match {
        case This(qual) if qual.decoded.startsWith("COVEN") => Ident(prefixVal)
        case other => super.transform(other)
      }
    }

    val (arg, body) = c.macroApplication.symbol.annotations.filter(
      _.tpe <:< typeOf[body]
    ).headOption.flatMap(
      _.scalaArgs.collectFirst {
        case Function(ValDef(_, arg, _, _) :: Nil, body) =>
          arg -> c.resetAllAttrs(body)
      }
    ).getOrElse(
      c.abort(c.enclosingPosition, "Annotation body not provided!")
    )

    c.Expr(
      q"""
        val $prefixVal = ${c.prefix}
        val $arg = $x
        ${replaceThises.transform(body)}
      """
    )
  }
}
```

Note that I've prefixed the name of the class with `COVEN` to allow
the transformer to avoid accidentally replacing other references to `this`.
If you have your own classes with names starting with `COVEN` you'll
want to pick a different prefix.

Once again we can show that this works:

``` scala
scala> val fooer = VampireExample.demo("foo")
fooer: AnyRef{val baz: Int; def foo(x: Int): Int} = $anon$1@4ddcf968

scala> fooer.foo(13)
res0: Int = 23
```

Is it pretty? No. But it's potentially very useful, and could be packaged up more nicely
with a little work.

