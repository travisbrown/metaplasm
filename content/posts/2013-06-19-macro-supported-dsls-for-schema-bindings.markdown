---
title: Macro-supported DSLs for schema bindings
date: Wed Jun 19 16:07:10 EDT 2013
tags: scala, macros, rdf, dh
---

We've recently started using the [W3C](https://www.w3.org/)'s
[`banana-rdf`](https://github.com/w3c/banana-rdf) library
at [MITH](http://mith.umd.edu/), and it's allowing us to make
a lot of our code for working with [RDF](http://www.w3.org/RDF/) graphs
both simpler and less tightly coupled to a specific RDF store. It's a
young library, but also very clever and well-designed, and it does an excellent job of
exploiting advanced features of the Scala language to make its
users' lives easier. [Alexandre Bertails](https://twitter.com/bertails)
and his collaborators deserve a lot of credit for what they've accomplished
in just a little over a year.

One of the least pleasant aspects of working with any RDF library is writing
bindings for particular vocabularies. For example, if we wanted to use the
[Open Archives Initiative](http://www.openarchives.org/)'s
[Object Reuse and Exchange](http://www.openarchives.org/ore/) vocabulary
in our `banana-rdf` application, we'd need to write something like the following:

<!-- MORE -->

``` scala
class OREPrefix[Rdf <: RDF](ops: RDFOps[Rdf])
  extends PrefixBuilder("ore", "http://www.openarchives.org/ore/terms/")(ops) {
  import ops._

  val aggregates = apply("aggregates")
  val isAggregatedBy = apply("isAggregatedBy")
  val describes = apply("describes")
  val isDescribedBy = apply("isDescribedBy")
  val lineage = apply("lineage")
  val proxyFor = apply("proxyFor")
  val proxyIn = apply("proxyIn")
  val similarTo = apply("similarTo")
  val Aggregation = apply("Aggregation")
  val AggregatedResource = apply("AggregatedResource")
  val Proxy = apply("Proxy")
  val ResourceMap = apply("ResourceMap")
}
```

This isn't as bad as writing a
[Jena vocabulary](http://jena.apache.org/documentation/javadoc/jena/com/hp/hpl/jena/vocabulary/package-summary.html),
for example, but it's still verbose and error-prone. A relatively simple application
can require dozens of these vocabulary bindings, some of which may define hundreds of classes and properties.
The vocabularies may also be under development
and subject to change, in which case the binding code must be kept in sync with a schema
or some other kind of documentation.

Since this is Scala, I decided that I'd take a stab at using macros to
make the process of writing these things a little easier. It's not
too hard to use [this trick](https://stackoverflow.com/q/14370842/334519) to write
a macro that will parse a given RDF schema at compile time and generate an
anonymous subclass of `Prefix` with the appropriate members. The usage looks like this:

``` scala
import org.w3.banana._, sesame._, edu.umd.mith.banana._

trait MyPrefixes[Rdf <: RDF] extends Prefixes[Rdf] {
  val ore = createFromSchema[Rdf](
    "http://www.openarchives.org/ore/terms/",
    "/ore-terms.rdf"
  )
}
```

And then:

``` scala
scala> val prefixes = new MyPrefixes[Sesame] { def ops = RDFOps[Sesame] }
prefixes: MyPrefixes[org.w3.banana.sesame.Sesame] = $anon$1@12928368

scala> prefixes.ore.similarTo
res0: org.w3.banana.sesame.Sesame#URI = http://www.openarchives.org/ore/terms/similarTo
```

No need to type out all the class and property names twice—we just drop the schema
in our resource folder, point to it when we call our macro method, and we're ready to go.
Note that we also don't have to provide the prefix name—it will be set automatically
based on the name of the value (`"ore"` in this case).

This is simple and easy, but it's not perfect. We don't have a "real" type for
our ORE `Prefix`—just the structural type of the `ore` instance. This can be inconvenient
in certain situations, and it also means that writing something like `ore.similarTo`
involves a reflective call. It's still entirely type-safe—we'd get a compile-time error if we accidentally wrote `ore.similarToo`,
for example—but it does inflict a small performance penalty, and if we don't want lots of warnings during compilation we
either have to set a compiler option to enable reflective calls or import `scala.language.reflectiveCalls` all over the place.

Someday we'll all live in [Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html)
and [type macros](http://docs.scala-lang.org/overviews/macros/typemacros.html) will make this kind of problem
completely irrelevant. In the meantime,
I've been experimenting with a couple of other approaches that avoid the problem
while still minimizing verbosity.
We have to write out all of our class and property names, but only once:

``` scala
trait OREPrefix[Rdf <: RDF] extends Prefix[Rdf] {
  val prefixIri = "http://www.openarchives.org/ore/terms/"

  val
    aggregates,  isAggregatedBy,
    describes,   isDescribedBy,
    lineage,     proxyFor,
    proxyIn,     similarTo,
    Aggregation, AggregatedResource,
    Proxy,       ResourceMap: Rdf#URI
}
```

Next we call a macro method that will create an instance of this trait
with the appropriate implementations of these values:

``` scala
trait MyPrefixes[Rdf <: RDF] extends Prefixes[Rdf] {
  val ore = create[OREPrefix]
}
```

And now we can run the REPL code above without any reflective calls. 

If we want a little more compile-time protection against typing mistakes,
we can point to a schema and the macro will confirm that all of our class
and property names are legitimate:

``` scala
trait MyPrefixes[Rdf <: RDF] extends Prefixes[Rdf] {
  val ore = createWithSchema[OREPrefix]("/ore-terms.rdf")
}
```

Suppose for example that this had been our trait:

``` scala
trait OREPrefix[Rdf <: RDF] extends Prefix[Rdf] {
  val prefixIri = "http://www.openarchives.org/ore/terms/"

  val these, arent, really, properties: Rdf#URI
}
```

We'd get a nice little error when we try to compile `MyPrefixes`:

``` scala
<console>:22: error: The following is not a valid property name: these
```

This is currently all experimental work, and I'm not sure whether any
of it will make its way into projects at MITH, but I've [published the
code on GitHub](https://github.com/umd-mith/banana-utils)
in case anyone's interested in taking a look at how it's implemented.

