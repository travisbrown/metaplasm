---
title: Deriving incomplete type class instances
date: Sun Jun 21 16:20:06 EDT 2015
tags: scala, finch, shapeless, scalaz
---

Suppose we've got a simple representation of a user:

```scala
case class User(id: Long, name: String, email: String)
```

Now suppose we're writing a web service where we allow clients to post some
JSON to a resource to create a new user. We get to pick the `id`, not the client,
so we might accept something like this:

```json
{
  "name": "Foo McBar",
  "email": "foo@mcbar.com"
}
```

If we're using a type class-based JSON library like [Argonaut][argonaut], we'll
probably have written a codec instance for `User` (or we may be using a library
like [argonaut-shapeless][argonaut-shapeless] that derives instances for our
case classes automatically).

The problem is that our `User` codec won't work on JSON like the
example above (since it's missing the `id` field).

<!-- MORE -->

One way around this issue
would be to write a new case class that represents a partial user:

```scala
case class PartialUser(name: String, email: String)
```

Or we could use a framework like [metarest][metarest] that would handle this
boilerplate for us. It'd be nice to have a more generic solution, though. One
possibility would be to have partial `DecodeJson` instances derived for us. In
our example above, this might look like the following:

```scala
import argonaut._, Argonaut._, Shapeless._

case class User(id: Long, name: String, email: String)

val json = """
  {
    "name": "Foo McBar",
    "email": "foo@mcbar.com"
  }
"""

Parse.decode[Long => User](json)
```

But this of course doesn't work, since argonaut-shapeless only generates
instances for things that have `LabelledGeneric` instances.

It is possible to make this happen, though—we just need something like this:

```scala
import scalaz.Functor
import shapeless._
import shapeless.ops.function.FnFromProduct

trait Incompletes[C[_]] {
  implicit def incompleteInstance[
    F,          // The `FunctionN` that we want an instance for.
    P <: HList, // The patch (possibly unlabeled).
    A,          // The case class (or whatever) that we're targeting.
    T <: HList, // The labeled representation of `A`.
    R <: HList  // The remaining fields.
  ](implicit
    ffp: FnFromProduct.Aux[P => A, F],
    gen: LabelledGeneric.Aux[A, T],
    complement: Complement.Aux[T, P, R],
    functor: Functor[C],
    instance: C[R]
  ): C[F] = functor.map(instance)(r =>
    ffp(p => gen.from(complement.insert(p, r)))
  )
}
```

Note that we get all of this stuff off the shelf with [Shapeless][shapeless]
(and [Scalaz][scalaz], which we're only using for its `Functor`) except for
`Complement` (more about that in a minute).

Now we can write something like this:

```scala
import argonaut.{ AutoDecodeJsons, AutoEncodeJsons, DecodeJson }

object ArgonautDerivation
  extends AutoDecodeJsons with AutoEncodeJsons
  with Incompletes[DecodeJson]
```

And then:

```scala
scala> case class User(id: Long, name: String, email: String)
defined class User

scala> Parse.decodeOption[Long => User](json).map(_(1001))
res0: Option[User] = Some(User(1001,Foo McBar,foo@mcbar.com))
```

Which is exactly what we wanted, with no boilerplate.

It also works with more complicated case classes (note the duplicate type):

```scala
case class User(id: Long, age: Long, name: String, email: String)

val lu = implicitly[DecodeJson[Long => User]]
```

Or with multiple missing pieces:

```scala
val lsu = implicitly[DecodeJson[(Long, String) => User]]
```

And if we really need to disambiguate missing fields with the same type, we can
use labels:

```scala
import shapeless.labelled.{ FieldType, field }

val json = """
  {
    "id": 1001,
    "name": "Foo McBar",
    "email": "foo@mcbar.com"
  }
"""

val withoutAge =
  Parse.decodeOption[FieldType[Witness.`'age`.T, Long] => User](json)
```

And then:

```scala
scala> withoutAge.map(_(field(25)))
res1: Option[User] = Some(User(1001,25,Foo McBar,foo@mcbar.com))
```

And we can accomplish all of this with no new macros and about a dozen lines of code,
thanks to Shapeless (if we don't count the `Complement` type class, which is a very
generic, slightly modified version of `ops.hlist.RemoveAll`).

A full demonstration project (including `Complement`) is available
[here][incompletes], and a version of `Incompletes` may be coming to either
[argonaut-shapeless][as15] or [Finch][finch] soon.

[argonaut]: http://argonaut.io/
[argonaut-shapeless]: https://github.com/alexarchambault/argonaut-shapeless
[as15]: https://github.com/alexarchambault/argonaut-shapeless/issues/15
[finch]: https://github.com/finagle/finch
[incompletes]: https://github.com/travisbrown/incompletes
[metarest]: https://github.com/pathikrit/metarest
[scalaz]: https://github.com/scalaz/scalaz
[shapeless]: https://github.com/milessabin/shapeless
