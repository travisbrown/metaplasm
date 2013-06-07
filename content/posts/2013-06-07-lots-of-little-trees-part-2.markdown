---
title: Lots of little trees, part 2
date: Fri Jun  7 17:08:52 EDT 2013
tags: xml, scala, java, nux, xom
---

I just noticed that the Lawrence Berkeley National Laboratory's
[Nux](http://acs.lbl.gov/software/nux/) library provides streaming
XQuery functionality that makes it very easy to do the kind of XML
processing that I described in [this post](http://meta.plasm.us/posts/2013/05/29/lots-of-little-trees/)
last week.

Using Scala, for example, we can start with some imports: 

``` scala
import nu.xom.{ Builder, Element, Nodes }
import nux.xom.xquery.{ StreamingPathFilter, StreamingTransform, XQueryUtil }
```

Next we write the "transformer" that we want to apply to every record element:

``` scala
val processor = new StreamingTransform {
  def transform(record: Element) = {
    val id = XQueryUtil.xquery(record, "IndexCatalogueID").get(0)
    val placeResults = XQueryUtil.xquery(record, "//Place")
    val places = (0 until placeResults.size) map placeResults.get

    println(id.getValue + " " + places.map(_.getValue).mkString(", "))
    new Nodes()
  }
}
```

We're not really tranforming anything here, of course—just performing a
side effect as we iterate through the records. We could just as easily
be adding some representation of the record to a mutable collection, sending a message
to an actor, etc.

<!-- MORE -->

Now we create and run our query:

``` scala
val recordPath = "/IndexCatalogueRecordSet/IndexCatalogueRecord"
val factory = new StreamingPathFilter(recordPath, null).createNodeFactory(null, processor)

new Builder(factory).build(new java.io.File("IndexCatalogueSeries1.xml"))
```

And we're done. Like my conduit-based implementation, this will iterate
through the records in a constant amount of memory. It's less elegant than
that solution, but it works, it's easy, and it seems to be significantly faster.

