---
title: The most horrible code I've ever written
date: Tue Aug 27 08:05:47 EDT 2013
tags: scala, macros
---

When macros first showed up in Scala as an experimental language feature last
year, many Scala developers
[responded with skepticism or distaste](http://blog.empathybox.com/post/19126121307/scala-macros-oh-god-why).
They argued that macros were a distraction from work on more urgent problems with
the language, that they would lead to even more complex and reader-unfriendly
code, etc. After a year and a half I think these arguments have less weight,
as macros have proven extremely useful in a wide range of applications:
[string interpolation](http://docs.scala-lang.org/sips/pending/string-interpolation.html),
[serialization](https://github.com/scala/pickling),
[type-level programming with singletons](https://github.com/milessabin/shapeless),
[numeric literals and faster loops](https://github.com/non/spire),
[typed channels for actors](http://doc.akka.io/docs/akka/snapshot/scala/typed-channels.html),
and so on. They've let me make many parts of my own code faster and safer
in surprising ways.

This post is not about a useful application of macros.
It's inspired by
[a couple](http://stackoverflow.com/q/18537093/334519)
[of questions](http://stackoverflow.com/q/18535356/334519) on Stack Overflow,
and it's an example of the kind of thing macros _should not_ be used for.
But it's Friday evening and I'm drinking beer in the office and I think this trick
is pretty clever, so here we go.

Suppose we've got a Scala class with some mutable fields:

``` scala
case class Car(var speed: Int, var color: String, var position: Int = 0)
```

Also suppose [we want](http://stackoverflow.com/q/18535356/334519)
to be able to write something like this:

``` scala
val car = new Car(0, "blue")

car set {
  color = "red"
  speed = 100
}
```

Something along these lines is apparently possible in charming languages
like Groovy and Visual Basic. We could get close in Scala, by adding a
`set` method to `Car` like this:

``` scala
def set(f: (Car) => Unit) = f(this)
```

And now we can write the following:

``` scala
car set { c =>
  c.color = "red"
  c.speed = 100
}
```

But suppose we're not satisfied with those nine or ten extra characters,
or that we don't want the overhead of the extra function application.
The problem is that `color` and `speed` doesn't mean anything on their own,
and we can't for example write a macro that would prepend `import car._` to
the block (as proposed [here](http://stackoverflow.com/q/18537093/334519)),
since the argument to the macro still needs to typecheck before the macro is expanded.

If we had [untyped macros](http://docs.scala-lang.org/overviews/macros/untypedmacros.html),
this would be easy, but they've been thrown out of paradise. We're not completely
out of luck with plain old `def` macros, though, since it's possible to use them to
introduce structural types with arbitrarily named and typed methods. This means
we can bring some dummy methods into scope that have the same shape as the 
setters for `Car`. The compiler will see these methods when it typechecks the macro
argument, and then our macro implementation can rewrite them to be calls to the
real setters on `Car`.

Before we see my implementation of this horrible idea, let's make the problem harder.
Suppose we also have some methods on `Car` that change its state:

``` scala
case class Car(var speed: Int, var color: String, var position: Int = 0) {
  def move(t: Int) = position += t * speed
}
```

We also want to be able to call these in our `set` block, like this:

``` scala
car set {
  color = "red"
  move(13)
}
```

This is so incredibly unpleasant I can't believe I'm even writing it.

