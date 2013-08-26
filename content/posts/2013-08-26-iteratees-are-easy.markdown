---
title: Iteratees are easy
date: Mon Aug 26 17:29:39 EDT 2013
tags: haskell, iteratees, streams
---

This blog post is a short response to my [MITH](http://mith.umd.edu/) colleague
[Jim Smith](http://www.jamesgottlieb.com/), who several weeks ago published
[a blog post](http://www.jamesgottlieb.com/2013/08/streams-part-ii/)
about a stream processing library that he's developing.
The post walks through an example of how this language
could allow you to take a stream of characters,
add some location metadata to each, and then group them into words, while still
holding onto the location metadata about the characters in the words.

The process he describes sounds a little like the functionality that [iteratees](http://okmij.org/ftp/Streams.html) provide,
so I decided I'd take a quick stab at writing up an
iteratee implementation of his example in Haskell.
I'm using [John Millikan](https://john-millikin.com/)'s
[enumerator](http://hackage.haskell.org/package/enumerator) package,
since that's the iteratee library that I'm most comfortable with.

<!-- MORE -->

We'll start with some imports, which you don't need to worry too much about at this point:

``` haskell
import Data.Char (isSpace)
import Data.Enumerator (($$), (=$), (=$=), enumList, run)
import qualified Data.Enumerator.List as L
```

<!--Next we'll define some types that correspond roughly to the annotated characters and
words in Jim's post:

``` haskell
type Loc = (Int, Int)
type LChar = (Char, Loc)
type LWord = (String, [Loc])
```

Here a location is just an integer representing the page number paired with one representing
the line, a located character is a character paired with a location, and a located string
is a string paired with a list of locations (one for each character in the string).-->

We need a few simple data structures in this program. We'll model locations as pairs
of integers representing page and line numbers. A located character will be a character
paired with a location, and a located word will be a string paired with a list of locations
(one for each character in the string). None of these types need any explicit definition.

Our first two functions will be very simple—they just tell us how to move to the next page
or line:

``` haskell
nextPage (page, _)    = (page + 1, 0)
nextLine (page, line) = (page, line + 1)
```

Each of these takes a location and returns a location. We could write out the type signatures,
but we don't have to, and these functions are pretty simple, so we won't.

Now for the first interesting part:

``` haskell
locator = L.mapAccum go (0, 0)
  where
    go loc c = let nextLoc = advance c loc in (nextLoc, (c, nextLoc))
    advance '\f' = nextPage
    advance '\n' = nextLine
    advance _    = id
```

I've skipped the unnecessary type signature, again—this time because it contains the word "monad".
Monads are neat, but they aren't terribly relevant here, and I'm not writing a monad tutorial.

The important thing to know about the type of `locator` is that it's an _enumeratee_.
An enumeratee is just a stream transformer—it plugs into a streaming source (or _enumerator_) on one end,
changes the items from that source in some way, and feeds them to a stream consumer (or _iteratee_).

The transformation may not be a one-to-one mapping—the enumeratee may take a dozen items from
its source and only feed one to its iteratee, for example. In this case, though, the enumeratee is
a simple mapping that takes each character and pairs it with its location. Note that this means
that the enumeratee has to maintain some state—this is why we use the `mapAccum` combinator instead
of just `map`.

Next we'll write another enumeratee to perform our tokenization:

``` haskell
tokenizer = L.splitWhen (isSpace . fst) =$= L.filter (not . null) =$= L.map unzip
```

Here we've composed three different enumeratees with the `=$=` combinator.
The first uses `splitWhen` to group incoming located characters into words, the second
drops empty words from the output stream, and the third "unzips" a list of located
characters into our located string type. The result is a stream transformer that
takes located characters on one end and outputs located strings on the other.

Next we need a source (or enumerator) that will stream characters into our enumeratee.
Let's use a Shelley poem:

``` haskell
poem = enumList 32 $
  "It is the same! — For, be it joy or sorrow,\n" ++
  "The path of its departure still is free:\f" ++
  "Man's yesterday may ne'er be like his morrow;\n" ++
  "Nought may endure but Mutability.\n"
```

Here we're enumerating characters from a string, but they could just as well be coming
from a file, a directory full of files, a network resource, etc.

Finally we can tie it all together:

``` haskell
main = run $ poem $$ locator =$= tokenizer =$ L.mapM_ print
```

That's a lot of combinators, but the idea is simple: we're running a pipeline that
has our poem as its source of characters, adds location metadata to each one, breaks
them into words, and feeds them to a consumer that just prints them to the screen.

The output looks like this:

``` haskell
("It",[(0,0),(0,0)])
("is",[(0,0),(0,0)])
("the",[(0,0),(0,0),(0,0)])
("same!",[(0,0),(0,0),(0,0),(0,0),(0,0)])
("\8212",[(0,0)])
("For,",[(0,0),(0,0),(0,0),(0,0)])
("be",[(0,0),(0,0)])
("it",[(0,0),(0,0)])
("joy",[(0,0),(0,0),(0,0)])
("or",[(0,0),(0,0)])
("sorrow,",[(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)])
("The",[(0,1),(0,1),(0,1)])
("path",[(0,1),(0,1),(0,1),(0,1)])
("of",[(0,1),(0,1)])
("its",[(0,1),(0,1),(0,1)])
("departure",[(0,1),(0,1),(0,1),(0,1),(0,1),(0,1),(0,1),(0,1),(0,1)])
...
```

I think it's pretty neat that we've been able to do this in under thirty lines of
pretty simple code, but the concision isn't the best part—I'd guess we could do
as well or better in languages like Python or Ruby.



