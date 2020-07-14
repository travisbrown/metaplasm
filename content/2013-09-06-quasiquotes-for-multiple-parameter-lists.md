+++
title = "Quasiquotes for multiple parameter lists"
original_date = 2013-09-06T08:00:29
path = "posts/2013/09/06/quasiquotes-for-multiple-parameter-lists"

[taxonomies]
tags = ["scala", "macros"]
+++

Quasiquotation is an old idea
(Miles Sabin [notes the term](https://twitter.com/milessabin/status/355088015577718785) in a 1937 Quine paper, for example)
that's now available in Scala (thanks to
[the efforts](http://infoscience.epfl.ch/record/185242) of
[Eugene Burmako](https://twitter.com/xeno_by) and
[Den Shabalin](https://twitter.com/den_sh)), where it allows
us to avoid the [nightmarishly complicated and verbose code](https://github.com/travisbrown/rillit/blob/61eed1c258f7a93d5370abacb8d043a082a3f1d2/core/src/main/scala/rillit/Lenser.scala)
that's required to construct abstract syntax trees manually in our macros.


Quasiquotes are a little like reification, but much more flexible about
what kinds of things can be "spliced" into the tree, and where they can be spliced. For example,
we couldn't use `reify` in the [following code](https://stackoverflow.com/q/14370842/334519), because there's no way to
splice in the name of the type member:

``` scala
def foo(name: String): Any = macro foo_impl
def foo_impl(c: Context)(name: c.Expr[String]) = {
  import c.universe._

  val memberName = name.tree match {
    case Literal(Constant(lit: String)) => newTypeName(lit)
    case _ => c.abort(c.enclosingPosition, "I need a literal!")
  }

  val anon = newTypeName(c.fresh)

  c.Expr(Block(
    ClassDef(
      Modifiers(Flag.FINAL), anon, Nil, Template(
        Nil, emptyValDef, List(
          constructor(c.universe),
          TypeDef(Modifiers(), memberName, Nil, TypeTree(typeOf[Int]))
        )
      )
    ),
    Apply(Select(New(Ident(anon)), nme.CONSTRUCTOR), Nil)
  ))
}
```

This is an unreadable mess, and it's not even a complete example—it depends
on some [additional utility code](https://gist.github.com/travisbrown/4552471).

<!-- more -->

Compare the version with quasiquotes:

``` scala
def foo(name: String): Any = macro foo_impl
def foo_impl(c: Context)(name: c.Expr[String]) = {
  import c.universe._

  val memberName = name.tree match {
    case Literal(Constant(lit: String)) => newTypeName(lit)
    case _ => c.abort(c.enclosingPosition, "I need a literal!")
  }

  c.Expr(q"new { type $memberName = Int }")
}
```

I know which version I'd prefer to maintain.

In general quasiquotation is this exactly this easy—you just use the `q`
string interpolator to plop your compile-time tree pieces into the code.
In some cases, though, the syntax required for quasiquoting isn't quite
the same as ordinary Scala syntax. Building a method with multiple parameter lists
is one example. Suppose we've got the following representation of our parameter lists:

``` scala
val paramss = List(
  List(newTermName("x") -> typeOf[Int], newTermName("y") -> typeOf[Char]),
  List(newTermName("z") -> typeOf[String])
)
```

And that we want to turn these into the following method:

``` scala
def foo(x: Int, y: Char)(z: String) = ???
```

We could write the following for a single list with a single parameter:

``` scala
q"def bar(${paramss.head.head._1}: ${paramss.head.head._2}) = ???"
```

But we can't for example write this (or any obvious variation of this)
to get both lists with all three parameters:

``` scala
val quoted = paramss.map(_.map { case (name, tpe) => q"$name: $tpe" })

q"def foo..${quoted.map(ps => q"($ps)")} = ???"
```

This was [confusing the hell out of me](https://stackoverflow.com/q/18559559/334519)
until yesterday Den [pointed out](https://stackoverflow.com/a/18644097/334519) the correct
syntax:

``` scala
val vparamss: List[List[ValDef]] = paramss.map(
  _.map { case (name, tpe) => q"val $name: $tpe" }
)

println(q"def foo(...$vparamss) = ???")
```


There are two things to note here. First, we need to use `q"val $name: $tpe"` instead
of `q"$name: $tpe"` (even though the former isn't valid Scala syntax for method parameters)
in order to make it clear that we want a `ValDef`, not a regular old typed expression.

Next,
we can't wrap each parameter list in parentheses and then quote the lot with `..$`.
Instead we quote the list of lists with `...$` inside a single pair of parentheses.

Once you've seen this syntax, it makes perfect sense, but it's maybe not exactly immediately obvious.

