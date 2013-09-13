---
title: Recursively recovering nesting
date: Fri Sep 13 10:06:14 EDT 2013
tags: scala, dh
---

My colleague [Jim Smith](https://twitter.com/jgsmith) and I have just been discussing
some code related to the [Shared Canvas](http://www.shared-canvas.org/) viewer that
we're working on for the [Shelley-Godwin Archive](http://shelleygodwinarchive.org/).
The goal of the code is to recover a hierarchical structure from a flattened list of
labeled ranges.

In Scala terms, we've got something like this (using Jim's example data):

``` scala
val ranges = Map(
  "apple"                 -> ( 0, 11),
  "banana"                -> (11, 20),
  "appleArtichoke"        -> ( 0,  3),
  "appleBanana"           -> ( 4,  7),
  "appleCranberry"        -> ( 8, 11),
  "appleArtichokeApricot" -> ( 0,  2),
  "appleArtichokeBBQ"     -> ( 3,  4),
  "appleBananaApricot"    -> ( 5,  7),
  "appleCranberryApricot" -> ( 8,  9),
  "appleArtichokeBBQ"     -> (10, 11)
)
```

And we want to recover a tree structure with a label at each node.
We'll assume here that the ranges don't overlap in ways that would break our hierarchy,
but explicitly validating this isn't too hard.

<!-- MORE -->

While looking at Jim's CoffeeScript implementation this morning, I realized that there's a very simple recursive solution (here in Scala, using [Scalaz](https://github.com/scalaz/scalaz)'s rose trees):

``` scala
import scalaz._, std.string._

def collectChildren(ranges: List[(String, (Int, Int))]): Stream[Tree[String]] =
  ranges match {
    case Nil => Stream.empty
    case (label, (_, last)) :: rest =>
      val (inside, outside) = rest.span(_._2._1 < last)
      Tree.node(label, collectChildren(inside)) #:: collectChildren(outside)
  }

def rangeNestings(ranges: Map[String, (Int, Int)]) = collectChildren(
  ranges.toList.sortBy {
    case (_, (first, last)) => (first, -last)
  }
)
```

This works as expected:

``` scala
scala> rangeNestings(ranges).foreach(tree => println(tree.drawTree))
"apple"
|
+- "appleArtichoke"
|  |
|  `- "appleArtichokeApricot"
|
+- "appleBanana"
|  |
|  `- "appleBananaApricot"
|
`- "appleCranberry"
   |
   +- "appleCranberryApricot"
   |
   `- "appleArtichokeBBQ"

"banana"
```

This struck me as an interesting kind of recursion—we're dividing the elements into
two groups with `span`, recursively processing the first group... 

