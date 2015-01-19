---
title: Vampire methods for structural types
date: Fri Jul 12 17:20:57 EDT 2013
tags: scala, macros
---

I wish I could take credit for what I'm about to show you, because it's
easily the cleverest thing I've seen all week,
but it's [Eugene Burmako's trick](https://twitter.com/xeno_by/status/355003437844398083)
and I've only simplified [his demonstration](https://gist.github.com/xeno-by/5967900)
a bit and adapted it to work in Scala 2.10.

<!-- MORE -->

First for the setup. Start your REPL like this to have it print the
tree for every expression after the compiler's `cleanup` phase:

``` bash
scala -Xprint:cleanup
```

It'll print some stuff you can ignore. Hit return to get a prompt (if you want), and then copy and paste the following:

``` scala
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.Context
 
class body(tree: Any) extends StaticAnnotation
  
object Macros {
  def makeInstance = macro makeInstance_impl

  def makeInstance_impl(c: Context) = c.universe.reify[Any] {
    class Workaround {
      def z: Int = 13
      @body(42) def v: Int = macro Macros.selectField_impl
    }
    new Workaround {}
  }

  def selectField_impl(c: Context) = c.Expr(
    c.macroApplication.symbol.annotations.filter(
      _.tpe <:< c.typeOf[body]
    ).head.scalaArgs.head
  ) 
}

val myInstance = Macros.makeInstance
```

And it'll print some more stuff you don't need to worry about.

The `makeInstance` method here is pretty simple:
we're just defining a class and instantiating it (using the workaround
I identified [here](https://stackoverflow.com/q/14370842/334519)).
The inferred type of the instance will be a structural type with `z` and `v`
methods.

And the punchline:

``` scala
trait Foo {
  val zombie = myInstance.z
  val vampire = myInstance.v
}
```

Now you can start paying attention to all that stuff it's been printing. Here's the important part:

``` scala
    def /*$read$$iw$$iw$Foo$class*/$init$($this: $line9.iw$Foo): Unit = {
      $this.$line9$$read$$iw$$iw$Foo$_setter_$zombie_=(scala.Int.unbox({
        val qual1: Object = $line8.$read$$iw$$iw.myInstance();
        try {
  $line9.$read$$iw$$iw$Foo$class.reflMethod$Method1(qual1.getClass()).invoke(qual1, Array[Object]{})
} catch {
  case (1 @ (_: reflect.InvocationTargetException)) => throw 1.getCause()
}.$asInstanceOf[Integer]()
      }));
      $this.$line9$$read$$iw$$iw$Foo$_setter_$vampire_=(42);
      ()
    }
```

This is what the trait's constructor looks like after the `cleanup` phase.
Notice all the ugly reflection business happening in the initialization of
`zombie`—this is why you get warnings about reflective access when you use
structural types in Scala, and why calling methods on structural types is
([at least a little](https://meta.plasm.us/posts/2013/07/11/fake-type-providers-part-2/))
slower.

Now look at the initialization for `vampire`.
No reflection at all—the macro has just replaced `myInstance.v` with `42`.

I missed this when Eugene first [posted it on Twitter](https://twitter.com/xeno_by/status/355003437844398083) a couple of days ago—now I wish I
could buy him a beer, because this totally made my Friday afternoon.

