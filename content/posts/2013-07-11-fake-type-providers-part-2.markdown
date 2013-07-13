---
title: Fake type providers, part 2
date: Thu Jul 11 19:11:35 EDT 2013
tags: scala, macros
---

I like writing code. I also like not writing code,
especially when I'm writing code.
[Type providers](http://scalamacros.org/usecases/type-providers.html)
are a particularly nice way not to write code. They let you take some
kind of schema (for a relational database, RDF vocabulary, etc.) and
turn it directly into binding classes at compile time—with no worrying
about managing generated code, etc.

I've wanted type providers in Scala for a long time (heck, I wanted
type providers ten years ago when I was a Java programmer who had no
idea what a type provider was but was dissatisfied with code generation
for this kind of task). The [type macros](http://docs.scala-lang.org/overviews/macros/typemacros.html)
currently available in [Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html)
will provide the real thing, but they're still (at least) months and months
away from a stable Scala release.

In the meantime, you can get surprisingly good fake type providers with
the `def` macros in Scala 2.10.
In [a previous post](/posts/2013/06/19/macro-supported-dsls-for-schema-bindings/)
I outlined one set of macro-based approaches to the problem, with the most concise version involving
structural types and therefore (unfortunately) reflective access.
In this post I'll go into a bit more detail about the code involved,
and will look at just how bad reflective access actually is performance-wise
by comparing the structural-type approach to two alternatives: plain old code
generation and macro-supported compile-time dynamic types.

<!-- MORE -->

First for the schema. I'll reuse the [ORE](http://www.openarchives.org/ore/)
example from my previous post, but with a simplified format instead of RDF Schema:

```
aggregates http://www.openarchives.org/ore/terms/aggregates
isAggregatedBy http://www.openarchives.org/ore/terms/isAggregatedBy
describes http://www.openarchives.org/ore/terms/describes
isDescribedBy http://www.openarchives.org/ore/terms/isDescribedBy
lineage http://www.openarchives.org/ore/terms/lineage
proxyFor http://www.openarchives.org/ore/terms/proxyFor
proxyIn http://www.openarchives.org/ore/terms/proxyIn
similarTo http://www.openarchives.org/ore/terms/similarTo
Aggregation http://www.openarchives.org/ore/terms/Aggregation
AggregatedResource http://www.openarchives.org/ore/terms/AggregatedResource
Proxy http://www.openarchives.org/ore/terms/Proxy
ResourceMap http://www.openarchives.org/ore/terms/ResourceMap
```

The format isn't terribly important—for this example we just need some kind of
mapping from names to URIs. One pair per line with a space separating key and value
is pretty damn simple and easy to parse, so that's what I'll go with here.

If you're following along at home, grab the lines above and stick them in a file
called `schema.txt` in whatever directory you want to work in, and you can copy
and paste the rest of the code below into a REPL that you start in that directory.

First suppose we have a trait that just marks that something is a schema:

```
trait Schema
```

If we were to write a Scala representation of the schema above by hand, it would look
like this:

``` scala
object oreGen extends Schema {
  import java.net.URI
  val aggregates = new URI("http://www.openarchives.org/ore/terms/aggregates")
  val isAggregatedBy = new URI("http://www.openarchives.org/ore/terms/isAggregatedBy")
  val describes = new URI("http://www.openarchives.org/ore/terms/describes")
  val isDescribedBy = new URI("http://www.openarchives.org/ore/terms/isDescribedBy")
  val lineage = new URI("http://www.openarchives.org/ore/terms/lineage")
  val proxyFor = new URI("http://www.openarchives.org/ore/terms/proxyFor")
  val proxyIn = new URI("http://www.openarchives.org/ore/terms/proxyIn")
  val similarTo = new URI("http://www.openarchives.org/ore/terms/similarTo")
  val Aggregation = new URI("http://www.openarchives.org/ore/terms/Aggregation")
  val AggregatedResource = new URI("http://www.openarchives.org/ore/terms/AggregatedResource")
  val Proxy = new URI("http://www.openarchives.org/ore/terms/Proxy")
  val ResourceMap = new URI("http://www.openarchives.org/ore/terms/ResourceMap")
}
```

But if we've got a lot of these things, writing the bindings by hand isn't going to be fun
and is likely to result in errors. So we write a code generator instead:

``` scala
trait SchemaParsingUtils {
  def parseLine(line: String): Either[String, (String, String)] =
    line.split(' ') match {
      case Array (k, v) => Right(k -> v)
      case _ => Left("Ill-formed schema line: " + line + "!")
    }
}

import scala.io.Source

object CodeGenSchemaMaker extends SchemaParsingUtils {
  def apply(path: String, name: String) = {
    val stream = Option(this.getClass.getResourceAsStream(path)).getOrElse(
      sys.error("Invalid resource path!")
    )

    val vals = Source.fromInputStream(stream).getLines.map(
      parseLine(_).fold(
        sys.error(_),
        { case (k, v) => "  val " + k + " = new URI(\"" + v + "\")\n" }
      )
    ).mkString

    s"object $name extends Schema {\n  import java.net.URI\n$vals}"
  }
}
```

And this works just fine—if we run the following, we should see the code
defining the `oreGen` object above:

``` scala
println(CodeGenSchemaMaker("/schema.txt", "oreGen"))
```

But now we have to incorporate this code-generation step
into our build process, etc. There are frameworks that can help with this, but 
it always still ends up feeling more or less ad-hoc and unpleasant.

So we decide to try compile-time metaprogramming. First for some imports and utilities:

``` scala
import scala.language.experimental.macros
import scala.reflect.macros.Context

trait ReflectionUtils {
  def constructor(u: scala.reflect.api.Universe) = {
    import u._
 
    DefDef(
      Modifiers(),
      nme.CONSTRUCTOR,
      Nil,
      Nil :: Nil,
      TypeTree(),
      Block(
        Apply(
          Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
          Nil
        ) :: Nil,
        Literal(Constant(()))
      )
    )
  }
}
```

And now for a simplified version of the structural-type approach in my previous post:

``` scala
object StructuralTypeSchemaMaker extends ReflectionUtils with SchemaParsingUtils {
  def apply(path: String) = macro apply_impl

  def apply_impl(c: Context)(path: c.Expr[String]): c.Expr[Schema] = {
    import c.universe._

    val stream = path.tree match {
      case Literal(Constant(pathLit: String)) =>
        Option(this.getClass.getResourceAsStream(pathLit)).getOrElse(
          c.abort(c.enclosingPosition, "Invalid resource path!")
        )
      case _ => c.abort(
        c.enclosingPosition,
        "You must provide a literal resource path for schema parsing!"
      )
    }

    val vals = Source.fromInputStream(stream).getLines.map(
      parseLine(_).fold(
        c.abort(c.enclosingPosition, _),
        {
          case (k, v) => ValDef(
            Modifiers(),
            newTermName(k),
            TypeTree(),
            reify(new java.net.URI(c.literal(v).splice)).tree
          )
        }
      )
    ).toList

    val anon = newTypeName(c.fresh())
    val wrapper = newTypeName(c.fresh())

    c.Expr(
      Block(
        ClassDef(
          Modifiers(),
          anon,
          Nil,
          Template(
            Ident(typeOf[Schema].typeSymbol) :: Nil,
            emptyValDef,
            constructor(c.universe) :: vals
          )
        ) ::
        ClassDef(
          Modifiers(Flag.FINAL),
          wrapper,
          Nil,
          Template(
            Ident(anon) :: Nil,
            emptyValDef,
            constructor(c.universe) :: Nil
          )
        ) :: Nil,
        Apply(Select(New(Ident(wrapper)), nme.CONSTRUCTOR), Nil)
      )
    )
  }
}
```

It's a lot of code, but the idea is pretty simple—we parse the schema file into a list
of `val`-definition ASTs, and then we stick these in an anonymous class that extends `Schema`.
This is in fact almost exactly what we did in the code-generation version, except that
here we're working with syntax trees instead of source code text, and we can't introduce
top-level definitions (like an object) in a macro—we can only define an anonymous class
and instantiate it.

We try it out:

``` scala
val oreStr = StructuralTypeSchemaMaker("/schema.txt")
```

And then:

``` scala
scala> oreStr.proxyIn
<console>:16: warning: reflective access of structural type member value proxyIn should be enabled
by making the implicit value language.reflectiveCalls visible.
This can be achieved by adding the import clause 'import scala.language.reflectiveCalls'
or by setting the compiler option -language:reflectiveCalls.
See the Scala docs for value scala.language.reflectiveCalls for a discussion
why the feature should be explicitly enabled.
              oreStr.proxyIn
                     ^
res0: java.net.URI = http://www.openarchives.org/ore/terms/proxyIn
```

So it works—but everyone who uses this code will either
have to import `language.reflectiveCalls` or put up with these stupid warnings.
See [that previous post](/posts/2013/06/19/macro-supported-dsls-for-schema-bindings/)
I keep mentioning for more discussion about the problems with reflective access here.

Yesterday [Eugene Burmako](http://xeno.by) [suggested on Twitter](https://twitter.com/xeno_by/status/354923990676025348)
that I try [`Dynamic`](http://www.scala-lang.org/api/current/index.html#scala.Dynamic) instead, so I did.
I'd always hated the idea of `Dynamic` in Scala before macros came along, but at compile-time
(i.e., implementing `selectDynamic` and friends with macros) it can actually be pretty nifty,
as [Aki Saarinen](http://akisaarinen.fi/) showed in his [Rillit](https://github.com/akisaarinen/rillit) lens library
(to take just one example).

Solving this problem with `Dynamic` is a little trickier than just defining and
instantiating an anonymous class. It's reasonable not to want the compile-time
representation of the schema (the `Map[String, String]` in this case) to exist
at runtime, and it's also reasonable to want to avoid parsing the schema more than
once. This means that somehow you have to share the compile-time schema representation between the macro
call that creates the schema instance, and the subsequent macro calls to `selectDynamic`
on that instance.

Last night I got hung up on the idea of [attachments](http://www.scala-lang.org/api/current/index.html#scala.reflect.macros.Attachments),
which allow you to attach arbitrary (type-indexed) metadata to trees or symbols.
It's a neat bit of macro functionality that I hadn't played with before,
but I'm pretty sure [it's not going to help in this case](http://stackoverflow.com/q/17580781/334519),
since I need to associate my compile-time schema representations with instances,
not trees or symbols.

If I'm wrong about this, [_please help me out_](http://stackoverflow.com/q/17580781/334519)!

Next [we start thinking crazy](https://twitter.com/milessabin/status/355073188893433857).
We can't attach stuff to the instance using attachments, and we can't access its value members for this
purpose (even if we wanted to, which we don't). So we make up a subclass of `Schema` with
a type parameter and fill it with the singleton type for a string literal
(we've got the schema path right there, so we'll use that) when we create our instance.
We also maintain a (mutable) map in our factory object that maps paths to schema representations.

``` scala
object DynamicSchemaMaker extends ReflectionUtils with SchemaParsingUtils {
  import scala.language.dynamics

  class DynamicSchema[P <: String] extends Schema with Dynamic {
    def selectDynamic(name: String) = macro DynamicSchemaMaker.selectDynamic[P]
  }

  val schemas = scala.collection.mutable.Map.empty[String, Map[String, String]]

  def apply(path: String) = macro apply_impl

  def apply_impl(c: Context)(path: c.Expr[String]): c.Expr[Schema] = {
    import c.universe._

    val pathLit = path.tree match {
      case Literal(Constant(pathLit: String)) => pathLit
      case _ => c.abort(
        c.enclosingPosition,
        "You must provide a literal resource path for schema parsing!"
      )
    }

    val stream = Option(this.getClass.getResourceAsStream(pathLit)).getOrElse(
      c.abort(c.enclosingPosition, "Invalid resource path!")
    )

    this.schemas(pathLit) = Source.fromInputStream(stream).getLines.map(
      parseLine(_).fold(c.abort(c.enclosingPosition, _), identity)
    ).toMap

    c.Expr(
      Apply(
        Select(
          New(
            TypeTree(
              appliedType(
                typeOf[DynamicSchema[_]].typeConstructor,
                ConstantType(Constant(pathLit)) :: Nil
              )
            )
          ),
          nme.CONSTRUCTOR
        ),
        Nil
      )
    )
  }

  def selectDynamic[P <: String: c.WeakTypeTag](c: Context)(name: c.Expr[String]) = {
    import c.universe._

    val schema = weakTypeOf[P] match {
      case ConstantType(Constant(path: String)) => schemas.getOrElse(
        path,
        c.abort(c.enclosingPosition, "This schema hasn't been parsed!")
      )
      case _ => c.abort(c.enclosingPosition, "Something really bad happened!")
    }

    val uri = name.tree match {
      case Literal(Constant(nameLit: String)) => schema.getOrElse(
        nameLit,
        c.abort(c.enclosingPosition, "Invalid member name!")
      )
      case _ => c.abort(
        c.enclosingPosition,
        "Invalid member name (it's not even a literal)!"
      )
    }

    reify(new java.net.URI(c.literal(uri).splice))
  }
}
```

And it actually works:

``` scala
scala> val oreDyn = DynamicSchemaMaker("/schema.txt")
oreDyn: DynamicSchemaMaker.DynamicSchema[String("/schema.txt")] = ...

scala> oreDyn.proxyIn
res1: java.net.URI = http://www.openarchives.org/ore/terms/proxyIn
```

No warnings about reflective access this time.

I'd originally intended to end this post with some nice [Caliper](https://code.google.com/p/caliper/) micro-benchmarks, but it's already
far too long and rambly, and I need a drink, so just do this:

``` scala
(0 until 10000000).reduceLeft((t, _) => t + oreGen.aggregates.toString.size)
(0 until 10000000).reduceLeft((t, _) => t + oreStr.aggregates.toString.size)
(0 until 10000000).reduceLeft((t, _) => t + oreDyn.aggregates.toString.size)
```

This is of course unscientific as hell, but run these a few times and it's pretty easy to convince yourself that the
`Dynamic` version is hugely slower—it takes seconds while the others return the result almost instantaneously.
If there's a difference between the code-generation version and the structural-type version, it's not remarkable.

Both of these facts are [a bit of a surprise](https://twitter.com/xeno_by/status/354936584652599296),
and I'm hoping to do some more digging in a future post.

