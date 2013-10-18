---
title: Explicit defaults in Scala
date: Thu Oct 17 21:10:00 EDT 2013
tags: scala, macros
---

This post is another entry in [my series](http://meta.plasm.us/posts/2013/08/30/horrible-code/) on
stuff you should never do with macros in Scala, but that you could do with macros in Scala, if you really wanted to,
and if you'd picked up a bottle of Macallan on the way home from work and were willing to waste half an
evening doing something ridiculously useless.

It's specifically a response to [this Stack Overflow question](http://stackoverflow.com/q/19432905/334519),
which asks if it's possible to specify explicitly that you want to use the default value of a constructor parameter in Scala.

So suppose we have a class like this:

``` scala
class Foo(val x: String, val y: Int = 13, val z: Symbol = 'zzz)
```

The goal is to allow the following syntax:

``` scala
val useDefaultZ = true

new Foo(x = "whatever", y = 1, z = if (useDefaultZ) default else 'whatever)
```

This _is_ possible with macros, and it's not nearly as easy as you might think.

<!-- MORE -->

Here's the code:

``` scala
import scala.language.experimental.macros
import scala.reflect.macros.Context

object DefaultFinder {
  def default: Any = macro default_impl

  def default_impl(c: Context): c.Expr[Any] = {
    import c.universe._

    c.enclosingUnit.body.collect {
      case Apply(Select(New(classId), nme.CONSTRUCTOR), args) =>
        args.collectFirst {
          case AssignOrNamedArg(Ident(argName), rhs)
            if rhs.exists(_.pos == c.macroApplication.pos) =>
              val tpe = c.typeCheck(tree = a, withMacrosDisabled = true).tpe

              tpe.declarations.collect {
                case m: MethodSymbol if m.isConstructor =>
                  m.paramss.headOption.flatMap { params =>
                    params.indexWhere(_.name == argName) match {
                      case -1 => None
                      case i =>
                        import scala.reflect.internal.{
                          Definitions,
                          SymbolTable,
                          StdNames
                        }

                        val u = c.universe.asInstanceOf[
                          Definitions with SymbolTable with StdNames
                        ]

                        val getter = u.nme.defaultGetterName(
                          u.nme.CONSTRUCTOR,
                          i + 1
                        )

                        Some(
                          c.Expr[Any](
                            Select(
                              Ident(tpe.typeSymbol.companionSymbol),
                              newTermName(getter.encoded)
                            )
                          )
                        )
                    }
                  }
              }.headOption.flatten
        }.flatten
    }.headOption.flatten.getOrElse(
      c.abort(c.enclosingPosition, "Sorry, you can't use default here!")
    )
  }
}
```

This is so horrible I'm not going to waste time commenting on it at all. But it works:

``` scala
scala> import DefaultFinder._
import DefaultFinder._

scala> new Foo(x = "whatever", y = 1, z = if (true) default else 'whatever)
res0: Foo = Foo@46635922

scala> res0.z
res1: Symbol = 'zzz

scala> new Foo(x = "whatever", y = default, z = default)
res2: Foo = Foo@7f9a454b

scala> res2.y
res3: Int = 13

scala> res2.z
res4: Symbol = 'zzz
```

It'll currently choke (fairly gracefully) on auxiliary constructors and constructors with multiple parameter lists,
and it won't work at all on any methods except constructors.
All these limitations could be addressed pretty easily,
but I'll leave that to someone who actually wants to use this awful syntax.

