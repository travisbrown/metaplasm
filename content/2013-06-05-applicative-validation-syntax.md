+++
title = "Applicative validation syntax"
original_date = 2013-06-05T19:00:38
path = "posts/2013/06/05/applicative-validation-syntax"

[taxonomies]
tags = ["scala", "scalaz", "shapeless"]
+++

People say that `Validation` is Scalaz's gateway drug,
which might be accurate if you ignore the suggestion that there's
anything even remotely fun about validation. In my book, making sure that
your program doesn't choke on bad input is always a chore.

Applicative validation is at least a step in the right direction—it makes it easier to
write less code, introduce fewer bugs, and draw clearer lines
between data models and validation logic. Suppose for example that we have the
following case class in Scala:

``` scala
case class Foo(a: Int, b: Char, c: String)
```

Suppose also that we have a form with three fields that we want to use to
create instances of `Foo`. We receive input from this form as
strings, and we want to be sure that these strings have certain properties.

<!-- more -->

Using [Scalaz 7](https://github.com/scalaz/scalaz), we can write the following:

``` scala
type ErrorsOr[A] = ValidationNel[String, A]
type Validator[A] = String => ErrorsOr[A]

val checkA: Validator[Int] = (s: String) =>
  try s.toInt.success catch {
    case _: NumberFormatException => "Not a number!".failureNel
  }

val checkB: Validator[Char] = (s: String) =>
  if (s.size != 1 || s.head < 'a' || s.head > 'z') {
    "Not a lower case letter!".failureNel
  } else s.head.success

val checkC: Validator[String] = (s: String) =>
  if (s.size == 4) s.success else "Wrong size!".failureNel

```

Now we can write a method that will lift our constructor into the
`ValidationNel` applicative functor and apply it to our input
(after running each piece of that input through the appropriate validator):

``` scala
def validateFoo(a: String, b: String, c: String) =
  (checkA(a) |@| checkB(b) |@| checkC(c))(Foo.apply _)
```

See my Stack Overflow answer [here](https://stackoverflow.com/a/12309023/334519) for some
additional discussion of this syntax (and applicative functors more generally) in the
context of validation.

We can confirm that this method works as expected:

``` scala
scala> println(validateFoo("ab", "cd", "ef"))
Failure(NonEmptyList(Not a number!, Not a lower case letter!, Wrong size!))

scala> println(validateFoo("42", "cd", "ef"))
Failure(NonEmptyList(Not a lower case letter!, Wrong size!))

scala> println(validateFoo("42", "x", "what"))
Success(Foo(42,x,what))
```

Unfortunately the `|@|` syntax is kind of ugly—especially compared to Haskell,
where we'd write the following:

``` haskell
validateFoo = Foo <$> checkA a <*> checkB b <*> checkC c
```

It gets even worse if you have more than twelve fields,
as [this Stack Overflow question](https://stackoverflow.com/q/16930347/334519) points out.
In that case [you have to fall back](https://stackoverflow.com/a/16943233/334519)
to calling `ap` (or, equivalently, Scalaz's version of `<*>`) directly, which looks like this:

``` scala
def validateFoo(a: String, b: String, c: String) =
  checkC(c) <*> (checkB(b) <*> (checkC(c) map (Foo.apply _).curried))
```

It's horrible—the arguments are in the wrong order and there are parentheses everywhere.

When I saw that question this morning, I started wondering about
ways that [Shapeless](https://github.com/milessabin/shapeless) might be able to
make the situation a little better. The rest of this post is a sketch of some
quick experiments in that direction.

First of all, it's not too hard to write a general operator for lifting
functions of arbitrary arity into any applicative functor:

``` scala
import scalaz._, Scalaz._
import shapeless._

object applier extends Poly2 {
  implicit def ap[F[_]: Applicative, H, T <: HList, R]:
    Pullback2Aux[applier.type, F[(H :: T) => R], F[H], F[T => R]] =
    at[F[(H :: T) => R], F[H]](
      (f, fa) => fa <*> f.map(hf => (h: H) => (t: T) => hf(h :: t))
    )
}

class Lifter[F[_]: Applicative] {
  def lift[G, H, A <: HList, M <: HList, R](g: G)(implicit
    hlG: FnHListerAux[G, A => R],
    mapped: MappedAux[A, F, M],
    unH: FnUnHListerAux[M => F[R], H],
    folder: LeftFolderAux[M, F[A => R], applier.type, F[HNil => R]]
  ) = unH((m: M) => folder(m, hlG(g).point[F]).map(_(HNil)))
}

def into[F[_]: Applicative] = new Lifter[F]
```

Now we can factor out the lifting part of our definition of `validateFoo`
in a way that's (arguably) a little nicer:

``` scala
val liftedFoo = into[ErrorsOr] lift (Foo.apply _)

def validateFoo(a: String, b: String, c: String) =
  liftedFoo(checkA(a), checkB(b), checkC(c))
```

One (possibly) more interesting approach would be to write a method
that would take a constructor and a heterogeneously-typed list
of validators and then build our top-level validation function for us.
This also isn't too hard. First for some more general-purpose machinery:

``` scala
def validate[F[_], G, H, V <: HList, I <: HList, M <: HList, A <: HList, R]
  (g: G)(v: V)(implicit
  hlG: FnHListerAux[G, A => R],
  zip: ZipApplyAux[V, I, M],
  mapped: MappedAux[A, F, M],
  unH: FnUnHListerAux[I => F[R], H],
  folder: LeftFolderAux[M, F[A => R], applier.type, F[HNil => R]],
  appl: Applicative[F]
) = unH((in: I) => folder(zip(v, in), hlG(g).point[F]).map(_(HNil)))
```

And now we can write the following:

``` scala
val validateFoo = validate(Foo.apply _)(checkA :: checkB :: checkC :: HNil)
```

Which we can use in exactly the same way as our original `validateFoo` above.

I'm not sure I want to argue that this approach is in any sense better than
going with the applicative builder syntax that comes with Scalaz (when that's an option), but it does give an example
of how we can use Shapeless to add flexibility to an existing library without a lot of boilerplate. 

