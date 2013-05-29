---
title: Lots of little trees
date: Wed May 29 11:32:16 EDT 20133
tags: xml, haskell, conduits, dh
---

In my field (computational humanities), people like to distribute databases as enormous XML files.
These are often very flat trees, with the root element containing hundreds of thousands or millions
of record elements, and can easily be too big to be parsed into memory as a DOM (Document Object Model) or
DOM-like structure.

This is exactly the kind of problem that streaming XML parsers are designed to solve.
There are two dominant approaches to parsing XML streams:
_push_-based models (like [SAX](http://en.wikipedia.org/wiki/Simple_API_for_XML), the "Simple API for XML"),
and _pull_-based models (like [StAX](https://sjsxp.java.net/), or—_shudder_—[`scala.xml.pull`](http://www.scala-lang.org/api/current/index.html#scala.xml.pull.package)).
Both of these approaches save memory by producing streams of _events_ (`BeginElement`, `Comment`, etc.)
instead of reconstructing a tree-based representation of the file in
memory (such a representation can be 5-10 times the size of the file on disk, which quickly becomes a problem
when you have four gigs of memory and your XML files are approaching a gigabyte in size).

<!-- MORE -->

Push-based APIs like SAX are inherently imperative: we register callbacks with the parser that specify how to handle events,
and then it calls them as it parses the XML file. With a pull parser, on the other hand, the programmer sees
the events as an iterator or lazy collection that he or she is responsible for iterating through.
Newer frameworks that support streaming XML parsing tend to provide pull-based APIs,
and many developers find pull parsing more intuitive (or at least slightly less miserable).

Either approach is reasonably convenient for the database-as-big-XML-file problem
when the record elements have a simple structure—if the record
elements are empty and fields are represented as attributes, for example.
As the record element schema becomes more complex (optional elements, deeper structures, etc.),
writing a streaming parser—either SAX or pull—quickly becomes painful.

One alternative approach would be to load the XML file into some kind of XML database
(like [BaseX](http://basex.org/), or [eXist](http://exist-db.org/exist/apps/homepage/index.html))
that would allow us to interact with the XML tree in a friendlier way, without needing
to represent the whole thing in memory. In many cases this is probably the right choice,
but it's a lot of overhead, and isn't the kind of thing I personally want to have to do
for a quick one-off data munging job.

The ideal solution would be something like what [Jackson](http://jackson.codehaus.org/)
provides [for JSON processing](http://www.cowtowncoder.com/blog/archives/2010/11/entry_434.html):

> For example, to process very large JSON streams, one typically starts with a
> streaming parser, but uses data binder to bind sub-sections of data into Java
> objects: this allows processing of huge files without excessive memory usage,
> but with full convenience of data binding.

[This Stack Overflow question](http://stackoverflow.com/q/16668649/334519)
suggests that there's no straightforward equivalent for XML in Java or Scala.

I've been working with [conduits](http://hackage.haskell.org/package/conduit) in Haskell recently,
and decided to take a stab at building on [xml-conduit](http://hackage.haskell.org/package/xml-conduit)'s
streaming XML parser to support this kind of multi-mode parsing.
My particular use case is the [IndexCat XML data](http://www.nlm.nih.gov/hmd/indexcat/indexcatxml.html)
distributed by the [U.S. National Library of Medicine](http://www.nlm.nih.gov/).

``` haskell
parsePlaces :: MonadThrow m => Consumer Event m (Maybe (T.Text, [T.Text]))
parsePlaces = fmap parseRecord <$> buildElementCursor
  where
    parseRecord cursor = (head recordId, places)
      where
        recordId = cursor $/ element "IndexCatalogueID" &/ content
        places = cursor $// element "Place" &/ content
```

