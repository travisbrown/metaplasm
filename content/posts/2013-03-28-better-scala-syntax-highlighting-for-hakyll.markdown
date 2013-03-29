---
title: Better Scala syntax highlighting for Hakyll
date: Thu Mar 28 21:35:58 EDT 2013
tags: scala, hakyll, pandoc
---

I chose [Hakyll](http://jaspervdj.be/hakyll/) over other static site generators for this blog in part
because Hakyll is built on [Pandoc](http://johnmacfarlane.net/pandoc/),
John MacFarlane's fantastic document conversion library.
Unfortunately Pandoc's syntax highlighting for Scala is incredibly bad:

<pre class="sourceCode scala"><code class="sourceCode scala"><span class="kw">import</span> scala.<span class="fu">language</span>.<span class="fu">experimental</span>.<span class="fu">macros</span>
<span class="kw">import</span> scala.<span class="fu">reflect</span>.<span class="fu">macros</span>.<span class="fu">Context</span>

<span class="kw">object</span> TupleExample {
  <span class="kw">def</span> fill[A](arity: Int)(a: A): Product = macro fill_impl[A]
  <span class="kw">def</span> fill_impl[A](c: Context)(arity: c.<span class="fu">Expr</span>[Int])(a: c.<span class="fu">Expr</span>[A]) = {
    <span class="kw">import</span> c.<span class="fu">universe</span>.<span class="fu">_</span>

    arity.<span class="fu">tree</span> <span class="kw">match</span> {
      <span class="kw">case</span> <span class="fu">Literal</span>(<span class="fu">Constant</span>(n: Int)) <span class="kw">if</span> n &lt; <span class="dv">23</span> =&gt; c.<span class="fu">Expr</span>(
        <span class="fu">Apply</span>(<span class="fu">Select</span>(<span class="fu">Ident</span>(<span class="st">&quot;Tuple&quot;</span> + n.<span class="fu">toString</span>), <span class="st">&quot;apply&quot;</span>), List.<span class="fu">fill</span>(n)(a.<span class="fu">tree</span>))
      )
      <span class="co">// Not going to worry about not getting what we expect.</span>
    }
  }
}</code></pre>

Note in particular that the line defining `fill` is entirely gray—at the very least
I'd expect the types to be highlighted somehow, and `macro` to be
treated as a keyword. I also have no idea why you'd want the first piece of
the package path in an `import` statement to be a different color from the rest of the path.

Pandoc uses the [Kate](http://kate-editor.org/) editor's syntax files,
and Kate's default syntax file for Scala is derived from one that's
[included in the Scala distribution](https://github.com/scala/scala-dist/blob/b885ebd4affe39f6713aa71b0df6257fb05adfe0/tool-support/src/kate/scala.xml).
We'll start by grabbing a copy of Kate's version of the file,
which is probably somewhere like this on your system:

``` bash
/usr/share/apps/katepart/syntax/scala.xml
```

The first thing you notice when you look at this file is that it starts off
with several thousand class names:

``` xml
<highlighting>
  <list name="scala2">
    <item> Actor </item>
    <item> ActorProxy </item>
    <item> ActorTask </item>
    ...
```

My guess is that these are there so that Kate can indicate when a class is in
the standard Scala or Java libraries (although I don't know why there would be
for example a `Fluid` in the Scala section, then). I don't particularly want
my syntax highlighting to worry about that kind of thing, and Pandoc ignores
the contexts that use these lists, anyway (since they only add extra styling
to the normal default style, and Pandoc apparently doesn't pass that along.)

So we just [scrap them](https://github.com/travisbrown/metaplasm/commit/7485bad573e1eb5cd737934fbd28df2a7e849ec5).
A little further down we see a similar list of Java's names for primitives (i.e., all lowercase).
That [can go](https://github.com/travisbrown/metaplasm/commit/1413c61b18cb6644b4954646c8040bd42a1f02ce),
too. Now our syntax file is 3,351 lines lighter, and it's easier to see how
[to add](https://github.com/travisbrown/metaplasm/commit/d103e75c4d8467f465d78b3e1ce454b3b2d80de6)
our macro keyword—it's just an `item` in the `keyword` list.

Kate's [Highlight Definition XML format](http://kate-editor.org/2005/03/24/writing-a-syntax-highlighting-file/)
is a little more verbose than e.g. Vim's language for doing the same thing, but it's pretty straightforward.
To handle our `import` problem, for example, we just add a line like this to the 
`Normal` context:

``` xml
<RegExpr attribute="Keyword" context="Imports" String="\b(package|import)\b" />
```

And then a new context like this: 

``` xml
<context attribute="Normal Text" lineEndContext="#pop" name="Imports">
  <DetectChar attribute="String" context="#pop" char=";"/>
</context>
```

And [that's all](https://github.com/travisbrown/metaplasm/commit/e6721aa1197d86795fdadcf1a8c321ae919e8f6e)—now
any `package` or `import` statement will have normal
text formatting up until we see a semicolon or the end of the line.

To highlight types, we start by adding a couple of lines to our `Normal` context again:

``` xml
<DetectChar attribute="Symbol" context="TypeParamTop" char="["/>
<RegExpr attribute="Symbol" context="TypeAnnot" String="[^:]?:\s"/>
```

And three new contexts to go with them:

``` xml
<context attribute="Data Type" lineEndContext="#stay" name="TypeParamTop">
  <DetectChar attribute="Data Type" context="TypeParam" char="["/>
  <DetectChar attribute="Symbol" context="#pop" char="]"/>
</context>
<context attribute="Data Type" lineEndContext="#stay" name="TypeParam">
  <DetectChar attribute="Data Type" context="TypeParam" char="["/>
  <DetectChar attribute="Data Type" context="#pop" char="]"/>
</context>
<context attribute="Data Type" lineEndContext="#stay" name="TypeAnnot">
  <DetectChar attribute="Data Type" context="TypeParam" char="["/>
  <RegExpr attribute="Data Type" context="#stay" String="\w+"/>
  <RegExpr attribute="Data Type" context="#stay" String="\s=>\s"/>
  <RegExpr attribute="Normal Text" context="#pop" String="[\)|\s]"/>
</context>
```

Now for the unpleasant part: getting Pandoc to see this syntax file.
Pandoc uses a library called [`highlighting-kate`](http://johnmacfarlane.net/highlighting-kate/)
(also developed by MacFarlane) that actually compiles Kate syntax files to Haskell code.
This means that we're going to have to rebuild both the
`highlighting-kate` and `pandoc` packages 

We start by unpacking the source for `highlighting-kate` (note that these instructions are based in part on
[a post by John Baker](http://bakerjd99.wordpress.com/2012/09/20/pandoc-based-j-syntax-highlighting/)
that explains how to add syntax highlighting for J to Pandoc):

``` bash
cabal unpack highlighting-kate-0.5.3.8
```

Note that I'm using `hakyll-4.2.2.0`, which depends on `pandoc-1.11.1`, which depends on `highlighting-kate-0.5.3.8`.
Your versions may vary.

The next step is to copy your new `scala.xml` file into the `xml` subdirectory of the `highlighting-kate-0.5.3.8` directory you just unpacked.
Then you build the thing, generate the Haskell code from the syntax files, and rebuild:

``` bash
cd highlighting-kate-0.5.3.8/
cabal configure
cabal build
runhaskell ParseSyntaxFiles.hs xml/
cabal build
cabal copy
```

And next for `pandoc` itself:

``` bash
cabal unpack pandoc-1.11.1
cd pandoc-1.11.1/
cabal configure
cabal build
cabal copy
```

Now you just rebuild your Hakyll project, and bingo:

``` scala
import scala.language.experimental.macros
import scala.reflect.macros.Context

object TupleExample {
  def fill[A](arity: Int)(a: A): Product = macro fill_impl[A]
  def fill_impl[A](c: Context)(arity: c.Expr[Int])(a: c.Expr[A]) = {
    import c.universe._

    arity.tree match {
      case Literal(Constant(n: Int)) if n < 23 => c.Expr(
        Apply(Select(Ident("Tuple" + n.toString), "apply"), List.fill(n)(a.tree))
      )
      // Not going to worry about not getting what we expect.
    }
  }
}
```

It's not perfect, but it _is_ vastly better,
and I don't really feel like spending any more time this evening fussing
with a syntax highlighter for an editor I don't even use. You can watch
[this space](https://github.com/travisbrown/metaplasm/blob/master/syntax/scala.xml)
for future improvements.

