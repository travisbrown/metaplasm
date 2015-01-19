---
title: Potemkin val-age
date: Mon Sep  2 11:46:10 EDT 2013
tags: scala, macros
---

My attempt to sneak the terms [_vampire_](https://meta.plasm.us/posts/2013/08/31/feeding-our-vampires/) and [_zombie_](https://meta.plasm.us/posts/2013/07/12/vampire-methods-for-structural-types/)
into the Scala vernacular [seems to be succeeding](https://github.com/scala/scala/pull/2902), so here's a new one:

> _Potemkin definitions_: definitions in a macro-constructed structural type that
> are intended only to make an expression passed as an argument to another macro method
> typecheck before that macro rewrites it.

I came up with the trick to support this [horrible abuse](https://meta.plasm.us/posts/2013/08/30/horrible-code/)
of Scala syntax:

``` scala
case class Car(var speed: Int, var color: String) { ... }
object Car { ... }

import Car.syntax._

val car = new Car(0, "blue")

car set {
  color = "red"
  speed = 10000
}
```

Here `color` and `speed` are definitions in a structural type that have the
same signatures as the fields on the case class, but they don't actually do
anything—if we call them we get a `NotImplementedError`. They only exist to
allow the block expression we're passing to `set` to typecheck before the
macro implementation replaces them with something useful.

<!-- MORE -->

This is a little like an [untyped macro](http://docs.scala-lang.org/overviews/macros/untypedmacros.html),
although instead of just turning off typechecking for the argument to `set`,
we're coming up with an elaborate lie to trick the compiler into signing off
on something that wouldn't make any sense otherwise.

