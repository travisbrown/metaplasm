---
title: Error accumulating decoders in circe
date: Thu Dec 17 09:24:02 EST 2015
tags: scala, json, circe
---

Suppose we're writing a service that accepts JSON requests and returns some kind of response. If
there's a problem with a request—it's not even valid JSON, it doesn't match the schema we expect,
etc.—we want to return an error, and of course it'd be nice if these errors were actually useful to
the caller.

Unfortunately "useful" in this context can mean lots of different things, and the differences will
usually depend at least in part on how involved a human was in creating the request. In the case of
validation errors—i.e. we successfully received some JSON, but it's not a shape we understand—then
if there's no human in sight, we generally only need to say something like "hey, we're not even
speaking the same language, you should probably go try somewhere else". A detailed breakdown of all
the reasons we don't understand the request is unlikely to be useful, so we might as well fail as
fast as possible and save resources.

If on the other hand a human was responsible for the content of the request, it's possible that
the caller will be able to make use of detailed information about all the problems with that
content. Suppose for example that the JSON comes from a web form or spreadsheet that for whatever
reason needs to be at least partially validated on the server side. In this case we probably don't
want to fail fast—we want to accumulate all of the errors and send them back together, so that the
human can correct them in a single pass.

<!-- MORE -->

## Error accumulation in Play JSON

This means that in many cases it's useful for a JSON decoder to have two modes for error reporting:
fail fast and error accumulating. In [Play JSON][play-json] (one of my favorite JSON libraries in
Scala), for example, we can determine whether we want a fail-fast or accumulating decoder by using
either monadic or applicative operations in our definition:

```scala
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class Foo(i: Int, s: String)

object Foo {
  val monadicReads: Reads[Foo] = for {
    i <- (__ \ 'i).read[Int]
    s <- (__ \ 's).read[String]
  } yield Foo(i, s)

  val applicativeReads: Reads[Foo] = (
    (__ \ 'i).read[Int] and (__ \ 's).read[String]
  )(Foo.apply _)
}
```

Which works like this:

```scala
val json = Json.parse("""{ "i": "abc", "s": 123 }""")

val JsError(monadicErrors) = json.validate(Foo.monadicReads)
val JsError(applicativeErrors) = json.validate(Foo.applicativeReads)
```

Now `monadicErrors` will be a sequence of length one (with the error indicating that the `i` field
isn't a number), while `applicativeErrors` will contain both errors.

This is great, but there are two arguments against this approach, one practical and the other more
theoretical. First of all, it means the creator of the decoder has to decide once and for all
whether or not the decoder accumulates errors. The library provides an accumulating decoder for
`List[String]`, for example, so in the following code we're going to end up with five errors:

```scala
val JsError(errors) = Json.parse("[1, 2, 3, 4, 5]").validate[List[String]]
```

There's no way to switch to fail-fast behavior if we don't need or want to pay the cost of
validating all the elements and collecting the errors into a sequence.

On the more theoretical side, people who care about these things tend to get grouchy about monadic
binding (`flatMap` here) being inconsistent with applicative composition. Any type constructor with
a monad instance also has an applicative functor instance, and the applicative operators can always
be written in terms of `map` and `flatMap`. Purists argue that you shouldn't ever be able to see
differences between a computation built up using applicative composition and an equivalent
computation using `map` and `flatMap`. Our `applicativeReads` above is equivalent to `monadicReads`
in this sense, so the fact that they fail differently is a problem (for these purists—I don't
personally care all that much, as we'll see in a minute).

## Error accumulation in Argonaut

I [love Argonaut][circe-why], but it basically punts on this issue. Not only does it not provide any
special support for defining error-accumulating decoders, it's not even really possible to
accumulate errors manually. You could do something like this if you were desperate:

```scala
import argonaut._, Argonaut._
import scalaz._, Scalaz._

case class Foo(i: Int, s: String)

object Foo {
  implicit val decoder: DecodeJson[Foo] = DecodeJson { c =>
    val iResult = (c --\ "i").as[Int]
    val sResult = (c --\ "s").as[String]

    (iResult.toDisjunction, sResult.toDisjunction) match {
      case (\/-(i), \/-(s)) => DecodeResult.ok(Foo(i, s))
      case (-\/((iMsg, history)), -\/((sMsg, _))) =>
        DecodeResult.fail(s"$iMsg, $sMsg", history)
      case (-\/((msg, history)), _) => DecodeResult.fail(msg, history)
      case (_, -\/((msg, history))) => DecodeResult.fail(msg, history)
    }
  }  
}
```

But this just mashes up the error messages and ignores all histories except the first (and it's also
just extremely unpleasant to write).

## Error accumulation in circe

I wanted to do better than this in [circe][circe], and this morning I merged a first draft of a
solution (thanks to a lot of [feedback from the community][circe-85] on the core idea and help from
[Pere Villega][pvillega] on the implementation).

In circe now anyone implementing `Decoder[A]` can optionally provide an implementation of the
following method (in addition to the required `HCursor => Xor[DecodingFailure, A]`):

```scala
import cats.data.ValidationNel
import io.circe._

// ...

private[circe] def decodeAccumulating(c: HCursor): ValidationNel[DecodingFailure, A]
```

This method is not part of the public API, but it supports conversion of any `Decoder` to a new
`AccumulatingDecoder` type. To show how this works, we can write the following:

```scala
import cats.syntax.apply._
import io.circe._, io.circe.jawn.decode

case class Foo(i: Int, s: String)

object Foo {
  val monadicDecoder: Decoder[Foo] = for {
    i <- Decoder[Int]
    s <- Decoder[String]
  } yield Foo(i, s)

  val applicativeDecoder: Decoder[Foo] =
    (Decoder[Int] |@| Decoder[String]).map(Foo.apply)
}
```

This is very similar to the Play code above, but these decoders both fail fast:

```scala
scala> import cats.data.{ Validated, Xor }
import cats.data.{Validated, Xor}

scala> val Xor.Right(json) = parse("""{ "i": "abc", "s": 123 }""")
json: io.circe.Json =
{
  "i" : "abc",
  "s" : 123
}

scala> Foo.monadicDecoder(json.hcursor)
res0: io.circe.Decoder.Result[Foo] = Left(io.circe.DecodingFailure: Int)

scala> Foo.applicativeDecoder(json.hcursor)
res1: io.circe.Decoder.Result[Foo] = Left(io.circe.DecodingFailure: String)
```

We can convert them into accumulating decoders, though:

```scala
val monadicAccDecoder = Foo.monadicDecoder.accumulating
val applicativeAccDecoder = Foo.applicativeDecoder.accumulating
```

And then we can see the difference:

```scala
val Validated.Invalid(monadicErrors) = monadicAccDecoder(json.hcursor)
val Validated.Invalid(applicativeErrors) = applicativeAccDecoder(json.hcursor)
```

Now `monadicErrors` contains only the first error and `applicativeErrors` contains both.

I'm not sure this will please all of the purists—monadic binding and applicative composition are
still inconsistent, we just can't see that without converting the decoders to an entirely different
type.

I care more about the fact that it means that the user gets to decide which behavior they want. If
the user never calls `accumulating`, the behavior will be exactly the same as Argonaut's: decoding
will fail immediately on the first error, and only the first error will be returned. If they want to
accumulate errors, though, they can call `accumulating` to get an accumulating decoder, and will
get as much accumulation as is possible.

The "as is possible" here is constrained by the use of monadic binding (or alternatively failure to
implement `decodeAccumulating` appropriately) in any decoder you use. As we can see in
`monadicDecoder` above, if you use `flatMap` (in this case via a `for`-comprehension), you lose
error accumulation. The data dependency in `monadicDecoder` isn't real—we don't actually need the
result of the `"i"` decoder to decode the `"s"`—but the compiler doesn't know that. Any `flatMap` at
any point "forgets" all accumulation after it, and the accumulating decoder we get from
`accumulating` will be limited to accumulation above the `flatMap`.

I don't foresee this being a problem in practice (although I could be wrong, of course). All of the
decoders provided by circe (including generically derived ones) avoid monadic binding when they can,
which means that something like the following just works:

```scala
import cats.data.{ Validated, Xor }
import io.circe._, io.circe.generic.auto._, io.circe.jawn.parse

case class Bar(i: Int, s: List[String])

val Xor.Right(json) = parse("""
  {
    "i": "abc",
    "s": ["foo", "bar", 123, "baz", 456]
  }
""")

val Validated.Invalid(errors) = Decoder[Bar].accumulating(json.hcursor)
```

We'll get all three errors, as expected.

This functionality is now available in the circe 0.3.0 snapshot, and while I'm sure it's not
perfect, I think it's solid enough for anyone who's interested to try it out. Existing code should
not be affected in any way (if it is, that's a bug). Any feedback on the design or implementation
would of course be greatly appreciated.

[argonaut]: http://argonaut.io/
[circe]: https://github.com/travisbrown/circe
[circe-85]: https://github.com/travisbrown/circe/pull/85
[circe-why]: https://github.com/travisbrown/circe#why
[play-json]: https://www.playframework.com/documentation/2.5.x/ScalaJson
[pvillega]: https://twitter.com/pvillega
