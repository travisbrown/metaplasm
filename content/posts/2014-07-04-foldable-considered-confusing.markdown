---
title: Foldable considered confusing
date: Fri Jul  4 10:08:34 EDT 2014
tags: scala, haskell
---

[Tomasz Nurkiewicz](https://twitter.com/tnurkiewicz) recently published
[an article](http://www.javacodegeeks.com/2014/06/option-fold-considered-unreadable.html)
arguing that the fold on `Option` (new in Scala 2.10) is unreadable and
inconsistent and should be avoided. I personally disagree about the unreadability part
and the _should be avoided_ part, which isn't too surprising, since I
[generally disagree with Tomasz](https://twitter.com/travisbrown/status/452412213509513217).
I have a lot of respect for him, though, and I can actually get on board with a
good chunk of what he says in the article.
I can understand why you might want to avoid `fold` in some projects, and I
agree that the way the standard library provides folds is inconsistent and
frustrating—I still get a little angry when I
think about the fact that `Try` doesn't have a fold even though it was introduced
in _the same version_ of the language that gave us the fold on `Option`.

So this post isn't about why Tomasz is wrong about readability, etc.—it's about how
much I hate the name `Foldable`.

<!-- MORE -->

Tomasz writes:

> `Option.fold()` takes just one parameter: current `Option` value! This breaks
> the consistency, especially when realizing `Option.foldLeft()` and
> `Option.foldRight()` have correct contract...

If we're thinking in terms of algebraic data types, though, it's just not really
the case that the fold on `Option` breaks consistency or is less correct than
the `foldRight` on sequences. The fact that the fold on `Option[A]` expects a
`B` and an `A => B` isn't an accident: these arguments precisely correspond to its two
constructors, `None` and `Some[A](x: A)`, just as the arguments to the `foldRight` on
`List` (a `B` and a `(A, B) => B`) match its constructors (`Nil` and `::[B](head: B, tl: List[B])`).

Tomasz goes on to contrast Scala's approach with Haskell's `Data.Foldable`:

> This is my biggest concern. I can't understand why _folding_ wasn't defined in
> terms of the type system (trait?)... [The] `Data.Foldable` typeclass describes
> various flavours of folding in Haskell...
> Haskell shows that folding (also over
> `Maybe`) can be at least consistent.

The problem is that the fold on `Option` has almost literally _nothing_ to do
with the `Foldable` type class. The [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia#Foldable) does
a good job of explaining this:

> The generic term "fold" is often used to refer to the more technical concept of
> catamorphism. Intuitively, given a way to summarize "one level of structure"
> (where recursive subterms have already been replaced with their summaries), a
> catamorphism can summarize an entire recursive structure.
> It is important to realize that `Foldable` does not correspond to catamorphisms,
> but to something weaker. In particular, `Foldable` allows observing only the
> left-right order of elements within a structure, not the actual structure itself.
> Put another way, every use of `Foldable` can be expressed in terms of `toList`.
> For example, `fold` itself is equivalent to `mconcat . toList`.

(As a side note, I have no idea who decided that the part of McBride
and Patterson's `Traversable` that can forget about shape should be called
`Foldable`—there's definitely no `Foldable` in either
[Applicative programming with effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.pdf) or
[The essence of the iterator pattern](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf).)

The ironic part is that the naming of `Data.Foldable` seems like a very Scala-y thing to do:
noticing some generalization and then co-opting a term from a closely related context where
it has an even more general meaning.

As Tomasz writes, this results in "a random name collision that doesn't bring anything
to the table, apart from confusion". We just have different perspectives on who's
doing the confusing—as far as I'm concerned, the fold on `Option` is a "real" fold,
and `Foldable` should have been named something like `Reduceable`.
