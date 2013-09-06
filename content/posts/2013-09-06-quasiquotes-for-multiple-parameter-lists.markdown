---
title: Quasiquotes for multiple parameter lists
date: Fri Sep  6 08:00:29 EDT 2013
tags: scala, macros, quasiquotes
---

Quasiquotation is an old idea
(Miles Sabin notes the term in [a 1937 Quine paper](https://twitter.com/milessabin/status/355088015577718785), for example)
that's now available in Scala (thanks to
[the efforts](http://infoscience.epfl.ch/record/185242) of
[Eugene Burmako](https://twitter.com/xeno_by) and
[Den Shabalin](https://twitter.com/den_sh)), where it allows
us to avoid the [nightmarishly complicated and verbose code](https://github.com/travisbrown/rillit/blob/61eed1c258f7a93d5370abacb8d043a082a3f1d2/core/src/main/scala/rillit/Lenser.scala)
that's required to construct abstract syntax trees manually.


Quasiquotes are a little like reification, but much more flexible about
what kinds of things can be "spliced" into the tree, and where. For example,
we couldn't use `reify` in the [following code](http://stackoverflow.com/q/14370842/334519), because there's no way to
splice in the name of the type member:

``` scala
def foo(name: String): Any = macro foo_impl
def foo_impl(c: Context)(name: c.Expr[String]) = {
  import c.universe._

  val Literal(Constant(lit: String)) = name.tree
  val anon = newTypeName(c.fresh)

  c.Expr(Block(
    ClassDef(
      Modifiers(Flag.FINAL), anon, Nil, Template(
        Nil, emptyValDef, List(
          constructor(c.universe),
          TypeDef(Modifiers(), newTypeName(lit), Nil, TypeTree(typeOf[Int]))
        )
      )
    ),
    Apply(Select(New(Ident(anon)), nme.CONSTRUCTOR), Nil)
  ))
}
```

This is an unreadable mess, and it's not even a complete example—it depends
on some [additional utility code](https://gist.github.com/travisbrown/4552471).

<!-- MORE -->

Compare the version with quasiquotes:

``` scala
def foo(name: String): Any = macro foo_impl
def foo_impl(c: Context)(name: c.Expr[String]) = {
  import c.universe._

  val Literal(Constant(lit: String)) = name.tree

  c.Expr(q"new { type ${newTypeName(lit)} = Int }")
}
```


