---
title: Lists are even easier
date: Tue Aug 27 08:05:47 EDT 2013
tags: haskell, scala
---

This is a quick follow-up to [my post last night](http://meta.plasm.us/posts/2013/08/26/iteratees-are-easy/)
about stream processing with iteratees. It's worth pointing out that we can
accomplish much the same thing even more concisely using Haskell's lists:

``` haskell
import Data.Char (isSpace)
import Data.List (mapAccumL)
import Data.List.Split (splitWhen)

nextPage (page, _)    = (page + 1, 0)
nextLine (page, line) = (page, line + 1)

locator = snd . mapAccumL go (0, 0)
  where
    go loc c = let nextLoc = advance c loc in (nextLoc, (c, nextLoc))
    advance '\f' = nextPage
    advance '\n' = nextLine
    advance _    = id

tokenizer = map unzip . filter (not . null) . splitWhen (isSpace . fst)

poem =
  "It is the same! - For, be it joy or sorrow,\n" ++
  "The path of its departure still is free:\f" ++
  "Man's yesterday may ne'er be like his morrow;\n" ++
  "Nought may endure but Mutability.\n"

main = mapM_ print $ tokenizer . locator $ poem
```

(We could make this even more concise by using the standard library's `words`
in our definition of `tokenizer`, but I'm trying to stick to the same basic
structure as the iteratee version.)

<!-- MORE -->

This will produce exactly the same output. The most obvious difference is that
there are fewer combinators—we're composing the pieces of our pipeline with
regular old function composition (`.`) and application (`$`) instead of crazy stuff
like `=$=` and `$$`.

In most cases this approach will work just fine, and it's even also streaming,
in the sense that the input list doesn't have to be fully evaluated before we
start, and could in fact be much too large to fit in memory.

This approach breaks down, however, when we're reading from resources like the
file system or the network. _Lazy I / O_ allows us to make it more or less work in these situations,
but can [lead to problems](http://stackoverflow.com/a/5892699/334519) (leaked file handles, etc.).
As I hope my previous post showed,
iteratees let us step around these problems without all that much extra syntactic overhead.

