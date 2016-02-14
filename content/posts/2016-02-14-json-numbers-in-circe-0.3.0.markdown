---
title: JSON numbers in circe 0.3.0
date: Sun Feb 14 11:30:06 CST 2016
tags: scala, circe, json, argonaut
---

I'm publishing this article as a blog post (rather than as part of the circe documentation) because
it's intended to be a discursive overview of the representation of JSON numbers in
[circe 0.3.0][circe-0.3.0], not as an up-to-date description of the implementation in whatever the
current circe version happens to be when you're reading this. For information about JSON numbers in
circe versions after 0.3.0, please see the project documentation (in particular the
[changelogs][changelogs] and [API docs][api-docs]).

<!-- MORE -->

## The syntax of JSON numbers

JSON numbers have a [fairly straightforward grammar][json-grammar]—you've got an optional sign, an
integral part, an optional decimal part, and an optional exponent (with 10 as the base). There are
some details about where e.g. `+` and `0` are allowed, but these are mostly uninteresting. Given a
representation (for example a 4-tuple of strings), writing a complete, correct, and reasonably
efficient parser for JSON number expressions is unlikely to take an experienced programmer more than
half an hour or so. In short: this part is boring and well-defined by the spec.

## Representing JSON numbers

Whether we're writing a JSON library or an application that processes JSON, we'll generally want
some kind of normalized form for JSON numbers that collapses distinctions between some pairs of
valid JSON number strings. For example, it's unlikely that we'll want to preserve a distinction
between these two expressions:

```json
1e100
1E100
```

Or these:

```json
100.0
100.00000000
```

Or maybe even these:

```json
100
1e2
```

Other cases are more difficult. What about numbers with and without decimal parts that are made up
entirely of zeros?

```json
100
100.00000000
```

Or signed zeros?

```json
0.0
-0.0
```

Or numbers that have the same double-precision floating-point representation?

```json
0.00...imagine a few hundred more zeros here...001
0.0
```

In circe only the last two cases are distinguished (assuming the JSON parser supports these
distinctions; Jawn does, and Scala.js supports signed zeros but does lose precision). The following
rules summarize the distinctions that circe supports:

* Precision is never lost (assuming the JSON parser doesn't lose it)
* If there's an exponent, the case of the `e` is irrelevant.
* More generally, whether the same number is written with an exponent or not is irrelevant.
* Negative and positive zero are different.

The general principle is that if there are reasonable use cases for making a distinction, circe
should support it. Signed zeros are useful for some numerical applications, so we preserve the sign.
If someone wants to make a case for distinguishing `100.0` and `100`, that could potentially happen
in a future version. Distinguishing `1e2` and `1E2` is probably a nonstarter.

## Implementing the JSON number representation

The last section says that circe _never_ loses precision, but clearly there have to be some limits
on the size of the numbers we can represent. According to the JSON grammar, JSON numbers can be
arbitrarily large—the grammar would happily accept a number with trillions of digits in the
exponent. [RFC 7159][rfc-7159] is a little more grounded:

> This specification allows implementations to set limits on the range and precision of numbers
> accepted.

circe follows Argonaut in aiming to make this limit really, really high—something more or less like
"does the expression fit in memory?". In both circe and Argonaut (6.1) we can do something like the
following:

```scala
import scalaz._, Scalaz._, argonaut.Parse

val \/-(x) = Parse.parse(s"""1e${ "9" * 1000 }""")
val \/-(y) = Parse.parse(s"""10e${ "9" * 999 }8""")

x.nospaces // will be s"""1e${ "9" * 1000 }"""
x === y    // will be true
```

In Argonaut (and circe before 0.3.0) this is accomplished by representing large numbers as a pair of
a `BigDecimal` and a `BigInt` exponent, with the `BigDecimal` either being zero or having a single
decimal digit to the right of the decimal point.

This works great for equality, but unfortunately that's all Argonaut uses this representation for.
If we try to do anything with these large number values except print them or compare them, they
start to break in different ways:

```scala
scala> x.number.map(_.toBigDecimal)
java.lang.NumberFormatException
  at java.math.BigDecimal.parseExp(BigDecimal.java:638)
  ...

scala> x.number.map(_.toLong)
java.lang.NumberFormatException
  at java.math.BigDecimal.parseExp(BigDecimal.java:638)
  ...
```

(On the current Argonaut head `toDouble` fails similarly, but this is a regression from 6.1, where
it returns positive infinity.)

Runtime exceptions are one thing, but it gets worse: user input can actually cause a thread to hang
pretty much forever:

```scala
Parse.parse("1e2147483647").map(_.number.map(_.toBigInt))
```

This attempts to create a `BigInt` with `Int.MaxValue` digits, which takes… a very, very long time.

circe 0.3.0 aims to make this situation less horrible by introducing a new big number type, which
I've named [`BiggerDecimal`][bigger-decimal]. This type is a lot like `java.math.BigDecimal` except
that the scale is a `BigInteger` instead of an `int`, and the unscaled value is constrained to have
no trailing zeros (for the sake of making equality easy to determine). It also provides a much more
limited set of operations than `BigDecimal`, and (most importantly) the operations it does provide
are guaranteed not to have godawful resource requirements.

All of this means that we can write the following in circe:

```scala
import cats.data.Xor, io.circe.jawn.parse

val Xor.Right(Some(x)) = parse(s"""1e${ "9" * 1000 }""").map(_.asNumber)
val Xor.Right(Some(y)) = parse("1e2147483647").map(_.asNumber)
```

And then:

```scala
scala> x.toBigDecimal
res0: Option[BigDecimal] = None

scala> x.toLong
res1: Option[Long] = None

scala> x.toDouble
res2: Double = Infinity

scala> y.toBigDecimal
res3: Option[BigDecimal] = Some(1E+2147483647)

scala> y.toBigInt
res4: Option[BigInt] = None
```

The conversions of `x` to `BigDecimal` and `y` to `BigInt` fail (immediately and safely) because
they are determined to be too expensive. You can still round-trip these values back to JSON, compare
them for equality, ask whether they're whole, etc.—you just can't convert them to these types.

The conversion of `x` to `Long` fails for a simpler reason—its value is outside the range of the
long integer type. In accordance with the horrible nature of `Double`, `x.toDouble` is more lossy
than the other operations—it returns the nearest `Double` value or one of the infinities if the
value is out of range.

## Representing JSON numbers: practical considerations

We could simply represent JSON numbers as `BiggerDecimal` values, but circe uses a slightly more
complex representation for practical reasons. If we're constructing a JSON number value from a
`Long` or `Double`, for example, we might as well make it possible to avoid converting those numbers
to `BiggerDecimal` values. The following is a simplified version of
[circe's JSON number][json-number] ADT (note that these constructors are not part of the public
API):

```scala
case class JsonDecimal(value: String) extends JsonNumber
case class JsonBiggerDecimal(value: BiggerDecimal) extends JsonNumber
case class JsonBigDecimal(value: BigDecimal) extends JsonNumber
case class JsonLong(value: Long) extends JsonNumber
case class JsonDouble(value: Double) extends JsonNumber
```

The `JsonDecimal` constructor is provided for cases where our parser hands us a string that has
already been validated as a JSON number and we want to parse it into a `BiggerDecimal` lazily. The
final three constructors are provided solely for the sake of efficiency.

## Decoding

One of the goals of circe is to make it possible for users to avoid ever interacting with types like
`Json` or `JsonNumber`. This means that everything above is (ideally) just a bunch of implementation
details. Typically users will work with JSON numbers by asking for them to be decoded into
meaningful types that have nothing to do with JSON. circe 0.3.0 introduces a few changes in this
respect.

The most important of these changes is a more clear-cut distinction between three groups of numeric
types. The first group is the arbitrary-precision types: `BigInt` and `BigDecimal`. circe's decoders
for these types will succeed under two conditions:

1. an exact representation is possible (e.g. non-whole JSON number will never be decoded into
   `BigInt` values); and
2. coming up with an exact representation wouldn't consume too many resources.

The second group is the integral types: `Byte`, `Short`, `Int`, and `Long`. In Argonaut (and in
circe before 0.3.0), the decoders for these types would happily truncate JSON number values:

```scala
scala> argonaut.Parse.decodeOption[Int]("0.99999999")
res0: Option[Int] = Some(0)

scala> argonaut.Parse.decodeOption[Short]("32768")
res1: Option[Short] = Some(32767)
```

This is no longer the case in circe—these decoders now succeed only if an exact representation is
possible.

```scala
scala> io.circe.jawn.decode[Int]("0.99999999")
res0: cats.data.Xor[io.circe.Error,Int] = Left(io.circe.DecodingFailure: Int)

scala> io.circe.jawn.decode[Short]("32768")
res1: cats.data.Xor[io.circe.Error,Short] = Left(io.circe.DecodingFailure: Short)
```

The third group is the floating-point types: `Float` and `Double`. These behave the same as they
always have—they will always succeed for any JSON number we can parse, but they do this by
truncating and losing precision:

```scala
scala> io.circe.jawn.decode[Double](s"""1e${ "9" * 1000 }""")
res2: cats.data.Xor[io.circe.Error,Double] = Right(Infinity)

scala> io.circe.jawn.decode[Double]("1e-10000000")
res3: cats.data.Xor[io.circe.Error,Double] = Right(0.0)
```

If you want to recover the old behavior for e.g. `Int`, it's of course possible to decode your JSON
into a `Double` and then truncate or round as desired.

## Conclusion

All of the above adds some complexity to circe's number implementation, but I think the result is a
much more consistent and reliable API. This is brand new work (I merged the `BiggerDecimal` stuff
yesterday morning and released circe 0.3.0 last night), so there may still be bugs (although the
test coverage is pretty good), and I'm sure there are aspects of the implementation that could be
improved. Any feedback—as a [GitHub issue][circe-gitter], in chat [on Gitter][circe-gitter],
complaints directed at me [on Twitter][twitter], etc.—would be greatly appreciated.

[api-docs]: https://travisbrown.github.io/circe/api/#io.circe.package
[bigger-decimal]: https://github.com/travisbrown/circe/blob/v0.3.0/numbers/shared/src/main/scala/io/circe/numbers/BiggerDecimal.scala
[changelogs]: https://github.com/travisbrown/circe/releases
[circe-0.3.0]: https://github.com/travisbrown/circe/releases/tag/v0.3.0
[circe-gitter]: https://gitter.im/travisbrown/circe
[circe-issues]: https://github.com/travisbrown/circe/issues
[json-grammar]: http://www.json.org
[json-number]: https://github.com/travisbrown/circe/blob/v0.3.0/core/shared/src/main/scala/io/circe/JsonNumber.scala
[rfc-7159]: https://tools.ietf.org/html/rfc7159
[twitter]: https://twitter.com/travisbrown
