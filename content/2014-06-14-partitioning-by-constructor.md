+++
title = "Partitioning by constructor"
original_date = 2014-06-14T01:05:06
path = "posts/2014/06/14/partitioning-by-constructor"

[taxonomies]
tags = ["scala", "shapeless", "macros"]
+++

It's not unusual in Scala to want to take a collection with items of some
algebraic data type and partition its elements by their constructors. In
[this Stack Overflow question](https://stackoverflow.com/q/24210197/334519),
for example, we're given a type for fruits:

``` scala
sealed trait Fruit

case class Apple(id: Int, sweetness: Int) extends Fruit
case class Pear(id: Int, color: String) extends Fruit
```

The goal is to be able to take a collection of fruits and split it into two
collections—one of apples and one of pairs.

``` scala
def partitionFruits(fruits: List[Fruit]): (List[Apple], List[Pear]) = ???
```

It's pretty easy to use `collect` to solve this problem for particular cases.
It's a little trickier when we start thinking about what a more generic version
of such a method would look like—we want to take a collection of items of some
arbitrary algebraic data type and return a _n_-tuple whose elements are
collections of items of each of that ADT's constructors (and let's require them
to be typed as specifically as possible, since this is Scala). It's not too hard
to imagine how you could write a macro that would perform this operation, but
it'd be messy and would probably end up feeling kind of ad-hoc (at least without
a lot of additional work and thinking).

Fortunately we've got [Shapeless 2.0](https://github.com/milessabin/shapeless),
where Miles Sabin and co. have written the macros for us so we can keep our
hands clean.

<!-- more -->

The key here is the `Generic` type class, which makes the coproduct-iness of the
ADT something we can work with explicitly:

``` scala
import shapeless._

trait Partitioner[C <: Coproduct] extends DepFn1[List[C]] { type Out <: HList }

object Partitioner {
  type Aux[C <: Coproduct, Out0 <: HList] = Partitioner[C] { type Out = Out0 }

  implicit def cnilPartitioner: Aux[CNil, HNil] = new Partitioner[CNil] {
    type Out = HNil

    def apply(c: List[CNil]): Out = HNil
  }

  implicit def cpPartitioner[H, T <: Coproduct, OutT <: HList](implicit
    cp: Aux[T, OutT]
  ): Aux[H :+: T, List[H] :: OutT] = new Partitioner[H :+: T] {
    type Out = List[H] :: OutT

    def apply(c: List[H :+: T]): Out =
      c.collect { case Inl(h) => h } :: cp(c.collect { case Inr(t) => t })
  }
}

def partition[A, C <: Coproduct, Out <: HList](as: List[A])(implicit
  gen: Generic.Aux[A, C],
  partitioner: Partitioner.Aux[C, Out],
  tupler: ops.hlist.Tupler[Out]
) = tupler(partitioner(as.map(gen.to)))
```

And now if we've got some fruits:

``` scala
val fruits: List[Fruit] = List(
  Apple(1, 10),
  Pear(2, "red"),
  Pear(3, "green"),
  Apple(4, 6),
  Pear(5, "purple")
)
```

We can write the following:

``` scala
scala> val (apples, pears) = partition(fruits)
apples: List[Apple] = List(Apple(1,10), Apple(4,6))
pears: List[Pear] = List(Pear(2,red), Pear(3,green), Pear(5,purple))
```

This is pretty neat—in less than thirty lines of code we've written a completely
generic partitioning method that'll take a collection of stuff of any algebraic
data type and split it up by constructor.

It's a little annoying that it's not immediately obvious how the tuple ended up
ordered that way, though. Do the apples come first because `Apple` was defined
first ([my preference](https://twitter.com/travisbrown/status/376738546901471232)),
or because it comes before `Pear` in the dictionary? If we were to go and dig
around in the Shapeless source code we'd learn that the constructors are sorted
by name, but we don't particularly want to have to remember that fact.

Fortunately Shapeless's records make a nicer syntax super easy:

``` scala
import shapeless._, labelled.{ field, FieldType }

trait Partitioner[C <: Coproduct] extends DepFn1[List[C]] { type Out <: HList }

object Partitioner {
  type Aux[C <: Coproduct, Out0 <: HList] = Partitioner[C] { type Out = Out0 }

  implicit def cnilPartitioner: Aux[CNil, HNil] = new Partitioner[CNil] {
    type Out = HNil

    def apply(c: List[CNil]): Out = HNil
  }

  implicit def cpPartitioner[K, H, T <: Coproduct, OutT <: HList](implicit
    cp: Aux[T, OutT]
  ): Aux[FieldType[K, H] :+: T, FieldType[K, List[H]] :: OutT] =
    new Partitioner[FieldType[K, H] :+: T] {
      type Out = FieldType[K, List[H]] :: OutT

      def apply(c: List[FieldType[K, H] :+: T]): Out =
        field[K](c.collect { case Inl(h) => (h: H) }) ::
        cp(c.collect { case Inr(t) => t })
  }
}

def partition[A, C <: Coproduct, Out <: HList](as: List[A])(implicit
  gen: LabelledGeneric.Aux[A, C],
  partitioner: Partitioner.Aux[C, Out]
) = partitioner(as.map(gen.to))
```

Note that apart from the imports, the first half of this block of code is
exactly the same as the one above. Instead of returning a tuple, though, we
return a [record](https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#extensible-records),
which is literally just an `HList` whose items are tagged with
labels. This gives us the following syntax:

``` scala
scala> val baskets = partition(fruits)
partitioned: shapeless.:: ...

scala> baskets('Apple)
res0: List[Apple] = List(Apple(1,10), Apple(4,6))

scala> baskets('Pear)
res1: List[Pear] = List(Pear(2,red), Pear(3,green), Pear(5,purple))
```

This is all completely type-safe—if we ask for a `baskets('Burger)` we'll
get a nice compile-time error. It's also generic enough that if we change our
code to add a banana constructor to our fruit type, we'll be able to write
`baskets('Banana)` here without touching the definition of `partition`.
