---
title: Iteratees are easy
date: Mon Aug 26 19:13:37 EDT 2013
tags: haskell, iteratees, streams
---

This blog post is a short response to my [MITH](http://mith.umd.edu/) colleague
[Jim Smith](https://twitter.com/jgsmith), who several weeks ago published
[a blog post](http://www.jamesgottlieb.com/2013/08/streams-part-ii/)
about a stream processing library that he's developing.
His post walks through an example of how this language
could allow you to take a stream of characters,
add some location metadata to each, and then group them into words, while still
holding onto the location metadata about the characters that make up the words.

The process he describes sounds a little like the functionality that [iteratees](http://okmij.org/ftp/Streams.html) provide,
so I decided I'd take a quick stab at writing up an
iteratee implementation of his example in Haskell.
I'm using [John Millikin](https://john-millikin.com/)'s
[enumerator](http://hackage.haskell.org/package/enumerator) package,
since that's the iteratee library that I'm most comfortable with.

<!-- MORE -->

We'll start with some imports, which you don't need to worry too much about at this point:

``` haskell
import Data.Char (isSpace)
import Data.Enumerator (($$), (=$), (=$=), enumList, run)
import qualified Data.Enumerator.List as L
```

We'll need a few simple data structures for this program. We'll model locations as pairs
of integers representing page and line numbers. A located character will be a character
paired with a location, and a located word will be a string paired with a list of locations
(one for each character in the string). None of these types need any explicit definition here,
although in a more complex program we'd probably want to give them names for clarity.

Our first two functions will be very simple—they just tell us how to move to the next page
or line:

``` haskell
nextPage (page, _)    = (page + 1, 0)
nextLine (page, line) = (page, line + 1)
```

Each of these takes a location and returns a location. We could write out the type signatures,
but we don't have to (thanks to Haskell's type inference), and these functions are pretty simple, so we won't.

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

The transformation doesn't have to be a one-to-one mapping—the enumeratee could take a dozen items from
its source and only feed one to its iteratee, for example. In this case, though, the enumeratee _is_
a simple mapping that takes each character and pairs it with its location. Note that the nature of
this task means that 
the enumeratee needs to maintain some state, which is why we use the `mapAccum` combinator instead
of just `map`.

Next we'll write another enumeratee to perform our tokenization:

``` haskell
tokenizer = L.splitWhen (isSpace . fst) =$= L.filter (not . null) =$= L.map unzip
```

Here we've composed three different enumeratees with the `=$=` combinator.
The first uses `splitWhen` to group incoming located characters into words, the second
drops empty words from the output stream, and the third "unzips" a list of located
characters into our located word type. The result is a stream transformer that
takes located characters on one end and outputs located words on the other.

Next we need a source (or enumerator) that will stream characters into our enumeratee.
Let's use a few lines from Shelley:

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

I think it's pretty neat that we've been able to do this in a few lines of
reasonably simple code, but the concision isn't the best part—I'd guess we could do
as well or better in languages like Python or Ruby.
The best part is how generic, composable,
safe, and efficient these components are. They're simple enough to write for a
little one-off parsing problem, but they can scale like crazy when your one-off
solution becomes a library. We could stream gigabytes of text through
the transformers we've defined here without worrying about memory usage at all.
If our enumerators are reading from the file system or the network,
we get lots of nice guarantees about resource management. If any part of our
pipeline can fail, we get nice clean ways to handle that failure. And so on.

And not a "monad" in sight.

