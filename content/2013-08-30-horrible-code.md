+++
title = "The most horrible code I've ever written"
original_date = 2013-08-30T17:52:19
path = "posts/2013/08/30/horrible-code"

[taxonomies]
tags = ["scala", "macros"]
+++

When macros first showed up in Scala as an experimental language feature last
year, many Scala developers
[responded with skepticism or distaste](http://blog.empathybox.com/post/19126121307/scala-macros-oh-god-why).
They argued that macros were a distraction from work on more urgent problems with
the language, that they would lead to even more complex and reader-unfriendly
code, etc.

After a year and a half I think these arguments have less weight,
as macros have proven extremely useful in a wide range of applications:
[string interpolation](http://docs.scala-lang.org/sips/pending/string-interpolation.html),
[serialization](https://github.com/scala/pickling),
[type-level programming with singletons](https://github.com/milessabin/shapeless),
[numeric literals and faster loops](https://github.com/non/spire),
[typed channels for actors](http://doc.akka.io/docs/akka/snapshot/scala/typed-channels.html),
and so on. They've let me make many parts of my own code faster and safer
in surprising ways.

This post is _not_ about a useful application of macros.
It's inspired by
[a couple](https://stackoverflow.com/q/18537093/334519)
[of questions](https://stackoverflow.com/q/18535356/334519) on Stack Overflow today,
and is an example of exactly the kind of thing macros _should not ever_ be used for.
But it's Friday evening and I'm drinking beer in the office and I think this trick
is pretty clever, so here we go.

<!-- more -->

Suppose we've got a Scala class with some mutable fields:

``` scala
case class Car(var speed: Int, var color: String, var position: Int = 0)
```

Also suppose we want to be able to write something like this:

``` scala
val car = new Car(0, "blue")

car set {
  color = "red"
  speed = 100
}
```

Apparently some kind of syntax along these lines is possible in charming languages
like Groovy and Visual Basic.

We can get close in Scala by adding a `set` method to `Car`:

``` scala
def set(f: (Car) => Unit) = f(this)
```

And now we can write the following:

``` scala
car set { c =>
  c.color = "red"
  c.speed = 100
}
```

But suppose we're not happy about those nine or ten extra characters,
or that we don't want the overhead of the extra function application.
The problem is that `color` and `speed` don't mean anything on their own.
The argument to the macro needs to typecheck before the macro is expanded, so
we can't for example write a macro that would just prepend `import car._` to
the block (as proposed [here](https://stackoverflow.com/q/18537093/334519)).

If we had [untyped macros](http://docs.scala-lang.org/overviews/macros/untypedmacros.html),
this would be easy, but they've been thrown out of paradise. We're not completely
out of luck with plain old `def` macros, though, since it's possible to use them to
introduce [structural types with arbitrarily named and typed methods](https://stackoverflow.com/q/14370842/334519).
This means
we can bring some dummy methods into scope that have the same shape as the 
setters for `Car`. The compiler will see these methods when it typechecks the macro
argument, and then our macro implementation can rewrite them to be calls to the
real setters on `Car`.

Before we see my implementation of this horrible idea, let's make the problem harder.
Suppose we also have some methods on `Car` that change its state:

``` scala
case class Car(var speed: Int, var color: String, var position: Int = 0) {
  def move(t: Int) = position += t * speed
}
```

And want to be able to call these in our `set` block, like this:

``` scala
car set {
  color = "red"
  move(13)
}
```

This is so incredibly unpleasant I can't believe I'm even writing it.


I'll start with the easy part—the macro implementation of `set`. Note that
I'm using quasiquotes, which are [now available as a plugin](http://docs.scala-lang.org/overviews/macros/paradise.html)
in Scala 2.10. Without them this post would fill a small book.

``` scala
import scala.reflect.macros.Context
import scala.language.experimental.macros

trait SetterBuilder {
  def set_impl(c: Context)(assignments: c.Expr[Unit]): c.Expr[Unit] = {
    import c.universe._

     val rewriteOne: PartialFunction[Tree, Tree] = {
       case q"${_}.$n($v)" => q"${c.prefix}.$n($v)"
     }

     val rewrite: PartialFunction[Tree, Tree] = rewriteOne orElse {
       case block: Block => q"{ ..${block collect rewriteOne} }"
     }

     c.Expr(
       rewrite.lift(assignments.tree).getOrElse(
         c.abort(c.enclosingPosition, "Not a set of assignments!")
       )
     )
  }
}
```

Pretty straightforward. We just check the argument for assignments and
try to rewrite them as assignments on the macro's prefix.

Next for the messier part—building the instance of the structural type that will give us
our dummy methods:

``` scala
trait SyntaxBuilder {
  def syntax_impl[A: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val anon = newTypeName(c.fresh())
    val declarations = c.weakTypeOf[A].declarations

    val (getters, setters) = declarations.collect {
      case sym: MethodSymbol if sym.isSetter => (
        q"def ${sym.getter.name} = ???",
        q"def ${sym.name}(x: ${sym.paramss.head.head.typeSignature}) = ???"
      )
    }.unzip

    val others = declarations.collect {
      case sym: MethodSymbol if sym.returnType =:= typeOf[Unit] && !sym.isSetter =>
        val params = sym.paramss.map(
          _.map(
            p => ValDef(
              Modifiers(Flag.PARAM),
              p.name.toTermName,
              TypeTree(p.typeSignature),
              EmptyTree
            )
          )
        )
        
        DefDef(Modifiers(), sym.name, Nil, params, TypeTree(), reify(???).tree)
    }

    c.Expr[Any](q"class $anon { ..$getters; ..$setters; ..$others }; new $anon {}")
  }
}
```

This wouldn't be quite so bad if I could be bothered to
figure out how to quasiquote multiple parameter lists;
at the moment I can't be.

Now we tie these pieces together:

``` scala
object Evil extends SyntaxBuilder with SetterBuilder {
  def syntax[A] = macro syntax_impl[A]
}
```

And then define our `Car` class:

``` scala
case class Car(var speed: Int, var color: String, var position: Int = 0) {
  def move(t: Int) = position += t * speed
  def set(assignments: Unit): Unit = macro Evil.set_impl
}

object Car {
  val syntax = Evil.syntax[Car]
}

import Car.syntax._
```

And we're done:

``` scala
scala> val car = new Car(0, "blue")
car: Car = Car(0,blue,0)

scala> car set {
     |   color = "red"
     |   speed = 10
     |   move(13)
     |   speed = 1
     |   move(42)
     | }

scala> car
res0: Car = Car(1,red,172)
```

It's so ridiculous. I love it.

