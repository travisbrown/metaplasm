+++
title = "Configuring generic derivation"
original_date = 2016-01-14T11:27:21
path = "posts/2016/01/14/configuring-generic-derivation"

[taxonomies]
tags = ["scala", "shapeless", "circe", "json", "macros"]
+++

This post is a kind of sequel to [my previous article on type classes and generic derivation][gd-part-1],
although unfortunately there's a lot of intermediate content that should go between there and here
that I haven't written yet. This post introduces a new feature in [circe][circe] that I'm pretty excited
about, though, so I'm not going to worry about skipping over that stuff for now.

<!-- more -->

One of the limitations of generic derivation in the current version of circe (0.2) is that it bakes
in a lot of decisions about the mapping that should be used between your types and JSON. For
example, suppose we've got an ADT like this (from the [San Francisco city lots tutorial][sf-city-lots]):

```scala
case class Coord(x: Double, y: Double)

sealed trait Geometry
case class Polygon(coordinates: List[List[Coord]]) extends Geometry
case class MultiPolygon(coordinates: List[List[List[Coord]]]) extends Geometry
```

We can derive decoders and encoders for these types with circe:

```scala
scala> import io.circe._, io.circe.generic.auto._, io.circe.syntax._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

scala> val p: Geometry = Polygon(List(List(Coord(0, 0), Coord(1, 0), Coord(1, 1))))
p: Geometry = Polygon(List(List(Coord(0.0,0.0), Coord(1.0,0.0), Coord(1.0,1.0))))

scala> println(p.asJson.noSpaces)
{"Polygon":{"coordinates":[[{"x":0.0,"y":0.0},{"x":1.0,"y":0.0},{"x":1.0,"y":1.0}]]}}
```

This is great, but what if we want to process incoming JSON that looks like this?

```json
{
  "type": "Polygon",
  "coordinates": [
    [{ "x": 0.0, "y": 0.0 }, { "x": 1.0, "y": 0.0}, {"x": 1.0, "y": 1.0 }]
  ]
}
```

The current answer is: you're out of luck and have to write your own by hand. This isn't too bad,
since we can use the derived instances for the ADT leaves:

```scala
implicit val decodeGeometry: Decoder[Geometry] = Decoder.instance(c =>
  c.downField("type").as[String].flatMap {
    case "Polygon" => c.as[Polygon]
    case "MultiPolygon" => c.as[MultiPolygon]
  }
)
```

But still, it would be nice to be able to configure the derivation a bit.

One common way to solve this kind of problem in Scala is to pass a configuration object implicitly
to the methods that provide our derived instances. Lots of Scala libraries and frameworks do it, and
lots of Scala developers seem happy with it as a solution, but I've always found it confusing and
error-prone. I don't want to have to reason about what bits of information about configuration I've
put into scope where, and as I say in the [circe design guidelines][design], I wanted to avoid
configuration-via-implicits until I was completely convinced there wasn't another solution.

I've just submitted a [pull request][circe-164] that proposes such a solution. Now if we've got our
`Geometry` type and a document like this:

```scala
val doc = """{
  "type": "Polygon",
  "coordinates": [
    [{ "x": 0.0, "y": 0.0 }, { "x": 1.0, "y": 0.0}, {"x": 1.0, "y": 1.0 }]
  ]
}"""
```

We can derive a configured decoder like this:

```scala
scala> import io.circe.generic.config._, io.circe.jawn._
import io.circe.generic.config._
import io.circe.jawn._

scala> decodeConfigured[Geometry, TypeField](doc).foreach(println)
Polygon(List(List(Coord(0.0,0.0), Coord(1.0,0.0), Coord(1.0,1.0))))
```

And print configured values like this:

```scala
scala> p.asJsonConfigured[TypeField]
res0: io.circe.Json =
{
  "type" : "Polygon",
  "coordinates" : [
    [
      {
        "x" : 0.0,
        "y" : 0.0
      },
      {
        "x" : 1.0,
        "y" : 0.0
      },
      {
        "x" : 1.0,
        "y" : 1.0
      }
    ]
  ]
}
```

The `TypeField` is a phantom type that tells circe to look for the discriminator in a field with the
key "type". It's provided for convenience, but if we want to use a different value for the key,
that's also pretty easy:

```scala
scala> p.asJsonConfigured[DiscriminatorField[Witness.`"I'll tell you what this is!"`.T]]
res1: io.circe.Json =
{
  "I'll tell you what this is!" : "Polygon",
  "coordinates" : [
    [
      {
        "x" : 0.0,
        "y" : 0.0
      },
      {
        "x" : 1.0,
        "y" : 0.0
      },
      {
        "x" : 1.0,
        "y" : 1.0
      }
    ]
  ]
}
```

There are currently four different options you can configure:

1. The ADT discriminator: should it be an object wrapper (the default) or a field?
2. Should case class member names be used verbatim as field names, or converted to snake case?
3. Should case objects in an ADT be represented by empty objects or just their names?
4. Should defaults values be used for case classes if they're available?

These options can be composed, so if we've got an ADT like this:

```scala
case class Foo(thisIsSomeNumber: Int = 0)

sealed trait MyAdt
case class Baz(someField: Foo, anotherField: List[Int] = Nil) extends MyAdt
case object Qux extends MyAdt
```

We can configure our decoder to use snake case for keys, to represent the case object as a string,
and to fill in missing fields with default values (if they exist):

```scala
scala> type C = SnakeCaseKeys with UseDefaultValues with CaseObjectString
defined type alias C

scala> decodeConfigured[MyAdt, C](""""Qux"""")
res2: cats.data.Xor[io.circe.Error,MyAdt] = Right(Qux)

scala> decodeConfigured[MyAdt, C]("""{ "Baz": { "some_field": {} } }""")
res3: cats.data.Xor[io.circe.Error,MyAdt] = Right(Baz(Foo(0),List()))
```

Now suppose that we want to use snake case keys for the members of `Foo`, but not `Baz`. This is
also pretty easy:

```scala
import io.circe.generic.semiauto._

case class Foo(thisIsSomeNumber: Int = 0)
object Foo {
  implicit val decodeFoo: Decoder[Foo] = deriveConfiguredDecoder[Foo, SnakeCaseKeys]
  implicit val encodeFoo: Encoder[Foo] = deriveConfiguredEncoder[Foo, SnakeCaseKeys]
}

sealed trait MyAdt
case class Baz(someField: Foo, anotherField: List[Int] = Nil) extends MyAdt
case object Qux extends MyAdt

type C = UseDefaultValues with CaseObjectString
```

And then:

```scala
scala> decodeConfigured[MyAdt, C]("""{
     |   "Baz": {
     |     "someField": { "this_is_some_number": 1000000 },
     |     "anotherField": [ 1, 2, 3 ]
     |   }
     | }""")
res4: cats.data.Xor[io.circe.Error,MyAdt] = Right(Baz(Foo(1000000),List(1, 2, 3)))
```

This overhaul of circe's generic derivation also happens to fix an [old issue][circe-69] where
derivation would fail for large case classes.

The implementation in the pull request still needs some tests and clean-up, but these configuration
options should be available in the circe 0.3.0 snapshot in the next couple of days.

[circe]: https://github.com/travisbrown/circe
[circe-69]: https://github.com/travisbrown/circe/issues/69
[circe-164]: https://github.com/travisbrown/circe/pull/164
[design]: https://github.com/travisbrown/circe/blob/master/DESIGN.md
[gd-part-1]: https://meta.plasm.us/posts/2015/11/08/type-classes-and-generic-derivation/
[sf-city-lots]: https://github.com/travisbrown/circe/tree/master/examples/sf-city-lots
