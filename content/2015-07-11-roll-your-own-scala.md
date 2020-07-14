+++
title = "Roll-your-own Scala"
original_date = 2015-07-11T09:33:55
path = "posts/2015/07/11/roll-your-own-scala"

[taxonomies]
tags = ["scala", "scalaz", "cats"]
+++

I've always really liked this passage from _On the Genealogy of Morals_:

> [T]here is a world of difference between the reason for something coming into
> existence in the first place and the ultimate use to which it is put, its
> actual application and integration into a system of goals… anything which
> exists, once it has come into being, can be reinterpreted in the service of
> new intentions, repossessed, repeatedly modified to a new use by a power
> superior to it.

A couple of months ago at [LambdaConf][lambdaconf] I had a few conversations
with different people about why we like (or at least put up with) Scala when
there are so many better languages out there. Most of the answers were the usual
ones: the JVM, the ecosystem, the job market, the fact that you don't have to
deal with Cabal, etc.

For me it's a little more complicated than that. I like Scala in part _because_
it's a mess. It's not a "fully" dependently typed language, but you can get
[pretty close](https://github.com/milessabin/shapeless) with singleton types and path dependent types. It provides
higher-kinded types, but you have to work around lots of bugs and gaps and
underspecified behaviors to do anything very interesting with them. And so
on—it's a mix of really good ideas and a few really bad ideas and you can put
them together in ways that the language designers didn't anticipate and probably
don't care about at all.

The rest of this blog post will be a long story about one example of this kind of
thing involving Scalaz's [`UnapplyProduct`](https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/Unapply.scala#L373).

<!-- more -->

## Type lambdas

Type lambdas are one example of a Scala "feature" that's both hugely useful (and
sometimes necessary) and apparently totally accidental. Suppose we've got a
simple functor type class:

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

And we want to create an instance for the `List` type constructor. This is easy:

```scala
implicit val listFunctor: Functor[List] = new Functor[List] {
  def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
}
```

Now suppose we want to create an instance for `Either[String, ?]`. This is a
little trickier. Scala doesn't provide a direct way to partially apply a
multi-parameter type constructor, but we can define a type alias with the
left-hand type filled in and then use that in our instance definition:

```scala
type StringOr[A] = Either[String, A]

implicit val stringOrXFunctor: Functor[StringOr] = new Functor[StringOr] {
  def map[A, B](fa: StringOr[A])(f: A => B): StringOr[B] = fa.right.map(f)
}
```

This trick won't work very well if we want the left-hand type to be generic,
though—there's simply no way to fit a type alias between the generic type
parameter on our defining method and that method's return type. We could try to
put the type alias inside the method and let the return type be inferred:

```scala
implicit def eitherFunctor[E] = {
  type EOr[A] = Either[E, A]

  new Functor[EOr] {
    def map[A, B](fa: EOr[A])(f: A => B): EOr[B] = fa.right.map(f)
  }
}
```

But that won't actually work:

```scala
scala> type StringOr[A] = Either[String, A]
defined type alias StringOr

scala> implicitly[Functor[StringOr]]
<console>:14: error: could not find implicit value for parameter e: Functor[StringOr]
       implicitly[Functor[StringOr]]
                 ^
```

Things are looking pretty bleak, but there _is_ a way we can define this
instance. It's just not the kind of thing you're likely to guess would work if
you hadn't seen it before.

```scala
implicit def eitherFunctor[E]: Functor[({ type L[x] = Either[E, x] })#L] =
  new Functor[({ type L[x] = Either[E, x] })#L] {
    def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa.right.map(f)
  }
```

This horrible syntax is an abuse of two Scala language features that at first
seem completely unrelated to the task at hand: type refinements and type
projections. These language features _seem_ completely unrelated to this problem
at least in part because they were designed for completely unrelated
purposes—see for example [this comment][si-6895-type-lambdas]
by Paul Phillips on a Scala issue I opened in 2012:

> Type lambdas are cool and all, but not a single line of the compiler was ever
> written with them in mind. They're just not going to work right: the relevant
> code is not robust.

This is one of the nice things about creating cool stuff by accident—you don't
have to support it (even when it becomes an idiomatic part of the language—type
lambdas for example have had [special support in IntelliJ for years][intellij],
and [kind-projector][kind-projector], a compiler plugin that provides syntactic
sugar for type lambdas, is becoming [increasingly widely used][scalaz-kp]).

## SI-2712

Now that we've got a functor instance for any `Either[E, ?]` we probably want to
be able to use it in methods like the following:

```scala
def incrementInside[F[_]: Functor](fa: F[Int]): F[Int] =
  implicitly[Functor[F]].map(fa)(_ + 1)
```

Which works like this on lists:

```scala
scala> val myList = List(1, 2, 3)
myList: List[Int] = List(1, 2, 3)

scala> val myIncrementedList = incrementInside(myList)
myIncrementedList: List[Int] = List(2, 3, 4)
```

And like this on `Either`:

```scala
val myEither: Either[String, Int] = Right(1)
val myIncrementedEither: Either[String, Int] = incrementInside(myEither)
```

Just kidding, that doesn't actually compile. Scala isn't capable of figuring out
that `Either[String, Int]` can be seen as a `F[Int]` where `F` is
`Either[String, ?]` (which of course we have a functor instance for).

This limitation is tracked in [SI-2712][si-2712], an issue that will celebrate
its sixth birthday this November (it's also one of the five or six Scala issues
that I can [recognize by number](https://twitter.com/travisbrown/status/612648222033510400)).

Fortunately there's (another) workaround, [due originally to Miles Sabin][unpackm]
and now available in both [Scalaz][scalaz-unapply] and [cats][cats-unapply]:

```scala
trait Unapply[TC[_[_]], MA] {
  type M[_]
  type A
  def instance: TC[M]
  def apply(ma: MA): M[A]
}

object Unapply {
  implicit def unapplyMA[TC[_[_]], M0[_], A0](implicit
    instance0: TC[M0]
  ): Unapply[TC, M0[A0]] {
    type M[X] = M0[X]
    type A = A0
  } = new Unapply[TC, M0[A0]] {
    type M[X] = M0[X]
    type A = A0
    def instance = instance0
    def apply(ma: M0[A0]): M[A] = ma
  }

  implicit def unapplyMAB[TC[_[_]], M0[_, _], A0, B0](implicit
    instance0: TC[({ type L[x] = M0[A0, x] })#L]
  ): Unapply[TC, M0[A0, B0]] {
    type M[x] = M0[A0, x]
    type A = B0
  } = new Unapply[TC, M0[A0, B0]] {
    type M[x] = M0[A0, x]
    type A = B0
    def instance = instance0
    def apply(ma: M0[A0, B0]): M[A] = ma
  }
}
```

Now we can rewrite our `incrementInside` method like this:

```scala
def incrementInside[FA](fa: FA)(implicit
  U: Unapply[Functor, FA] { type A = Int }
): U.M[Int] = U.instance.map(U(fa))(_ + 1)
```

And then:

```scala
scala> incrementInside(myList)
res4: List[Int] = List(2, 3, 4)

scala> incrementInside(myEither)
res5: scala.util.Either[String,Int] = Right(2)
```

Fortunately in practice we usually only need to define methods that take an
`Unapply` instance for a small handful of generic operations (a common
convention is to add a `U` to the end of the method name to distinguish these
versions—so for example in Scalaz the syntax for `Traverse` provides both
`traverse` and `traverseU` methods).

So now we've used implicit resolution to work around a limitation involving
higher-order unification for a type class instance that we defined using type
refinements and type projections to work around a limitation involving partial
application of type parameters. Next we'll add even more dead body parts to the
monster.

## Multiple implicit parameter sections

Sometimes we need more complex `Unapply` machinery than the relatively simple
version above. One example is syntax for things that are bitraversable. For
example, we might want to take a tuple (or `Either`) and do some kind of logging
in a `Writer` monad for both sides (or either):

```scala
import scalaz._, Scalaz._

(1, 'a).bitraverseU(_.set("saw int" :: Nil), _.set("saw sym" :: Nil))
```

This in effect lets us map two functions returning writers over the two sides of
our tuple and then turn the whole thing inside out, giving us a
writer that logs to a list of strings and returns a `(Int, Symbol)`.
We need the `bitraverseU` version again
because of SI-2712—the Scala compiler isn't able to recognize the pieces it
needs in the return types of the functions we're mapping over the two sides.

Unfortunately this doesn't actually work in Scalaz at the moment. It relies on an
`Unapply` variant called `UnapplyProduct` that was added to Scalaz in 2011 by
Jason Zaugg, but it's always been necessary to construct these
instances by hand (which is extremely inconvenient and boilerplate-y). From
Jason's original comments, which are still in the source today:

```scala
// This seems to motivate multiple implicit parameter sections. Is there another way?
```

And:

```scala
// Would be nice, but I'm not sure we can conjure UnapplyProduct implicitly, at least without multiple implicit
// parameter lists.
```

The issue is that the signature for the implicit method that generates
`UnapplyProduct` instances seems to need to look like the following, since we
have to confirm that the `M` types in our `Unapply` instances for the left and
right sides are the same:

```scala
implicit def unapply[TC[_[_]], MA0, MB0](implicit
  U1: Unapply[TC, MA0],
  U2: Unapply[TC, MB0]
)(implicit iso: U1.M <~> U2.M): UnapplyProduct[TC, MA0, MB0] { // ...
```

Scala doesn't allow types to be referred to by other types in the same parameter
list, so we can't just stick the `<~>` in with the other instances. The problem
is that Scala also doesn't support multiple implicit parameter lists. This is
a more arbitrary limitation, but I've never seen any serious discussion of
changing it.

So we're out of luck. I should know by now that if Jason Zaugg is asking for
help on a problem, I should stay away from it, but over the past three or four
years I've burned at least two or three Saturdays trying to find a way to
implement this `unapply` method.

## More workarounds!

In February Miles Sabin posted [a gist](https://gist.github.com/milessabin/cadd73b7756fe4097ca0)
demonstrating what he called "a new approach to encoding dependently-typed
chained implicits, using singleton types". It's a pretty clever trick, and my
first thought when I saw it was that we could use it to fix Scalaz's `UnapplyProduct`. And it
[worked](https://twitter.com/travisbrown/status/570593922797457408), although I
didn't get around to submitting a pull request until [this week](https://twitter.com/travisbrown/status/618938632787857408).

The implementation looks like this:

```scala
case class SingletonOf[T, U <: { type A; type M[_] }](
  widen: T { type A = U#A; type M[x] = U#M[x] }
)

object SingletonOf {
  implicit def mkSingletonOf[T <: { type A; type M[_] }](implicit
    t: T
  ): SingletonOf[T, t.type] = SingletonOf(t)
}

implicit def unapply[
  TC[_[_]],
  MA0,
  MB0,
  U1 <: { type A; type M[_] },
  U2 <: { type A; type M[_] }
](implicit
  sU1: SingletonOf[Unapply[TC, MA0], U1],
  sU2: SingletonOf[Unapply[TC, MB0], U2],
  iso: U1#M <~> U2#M
): UnapplyProduct[TC, MA0, MB0] {
  type M[x] = U1#M[x]
  type A = U1#A
  type B = U2#A
} = new UnapplyProduct[TC, MA0, MB0] {
  type M[x] = U1#M[x]
  type A = U1#A
  type B = U2#A
  def TC = sU1.widen.TC
  def _1(ma: MA0) = sU1.widen(ma)
  def _2(mb: MB0) = iso.from(sU2.widen(mb))
}
```

The key idea is that the `U1` and `U2` type parameters will be inferred to be
the singleton types for our two `Unapply` instances, and our `SingletonOf` type
class captures the way that these types line up with our `TC`, `MA0`, and `MB0`.
This allows us to refer to the `M` type parameters for the `Unapply` instances in
the same implicit parameter list that gathers the `Unapply` instances themselves
(or rather their `SingletonOf` wrappers).

Note that `SingletonOf` here is less generic than Miles's original formulation,
which would also work if we were willing to do some (safe) casting. Since this
is the only interesting application that I can think of for this trick, I'm not worried about
using a less generic formulation.

To demonstrate that it works, you can check out my [pull request](https://github.com/scalaz/scalaz/pull/969)
and run something like the following:

```scala
import scalaz._, Scalaz._

val w = (1, 'a).bitraverseU(_.set("saw int" :: Nil), _.set("saw sym" :: Nil))
```

And then:

```scala
scala> w.run
res0: (List[String], (Int, Symbol)) = (List(saw int, saw sym),(1,'a))
```

To recap: we've taken a few basic (but still pretty broken) Scala language
features—singleton types, refinement types, type projections, and the implicit
resolution system—and we've very painfully built a language for ourselves that
at least kind of looks like it supports partial type parameter application,
higher-order unification, and multiple implicit parameter sections.

I think that's pretty neat, but I can also understand why almost everyone else
would find it horrifying.

[cats-unapply]: https://github.com/non/cats/blob/master/core/src/main/scala/cats/Unapply.scala
[intellij]: http://stackoverflow.com/a/8737611/334519
[kind-projector]: https://github.com/non/kind-projector
[lambdaconf]: http://www.degoesconsulting.com/lambdaconf-2015/
[scalaz-kp]: https://github.com/scalaz/scalaz/pull/875
[scalaz-unapply]: https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/Unapply.scala
[si-2712]: https://issues.scala-lang.org/browse/SI-2712
[si-6895-type-lambdas]: https://issues.scala-lang.org/browse/SI-6895?focusedCommentId=61697&page=com.atlassian.jira.plugin.system.issuetabpanels:comment-tabpanel#comment-61697
[unpackm]: https://issues.scala-lang.org/browse/SI-2712?focusedCommentId=55239&page=com.atlassian.jira.plugin.system.issuetabpanels:comment-tabpanel#comment-55239
