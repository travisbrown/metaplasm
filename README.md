Metaplasm
---------

I'm trying out [Hakyll 4](http://jaspervdj.be/hakyll/)
and playing with [Bootstrap](http://twitter.github.com/bootstrap/)
and [Modernizr](http://modernizr.com/) integration.

Bits of the Hakyll machinery are adapted from sites by
[Ian Ross](https://github.com/ian-ross/blog) and
[Jasper Van der Jeugt](https://github.com/jaspervdj/jaspervdj),
and the current design is inspired by Lucas Lew's
[Whitespace](https://github.com/lucaslew/whitespace) theme for [Octopress](http://octopress.org/).

Note that you'll need to clone the project recursively to pull in the
submodules:

``` bash
git clone --recursive git@github.com:travisbrown/metaplasm.git
cd metaplasm
stack setup
stack build
stack exec site build
```

If you want nicer Scala syntax highlighting, you'll need a few extra steps:

``` bash
cp syntax/scala.xml lib/highlighting-kate/xml/
cd lib/highlighting-kate
stack runghc --package parsec --package hxt --package utf8-string --package regex-posix ParseSyntaxFiles.hs xml/
cd ../..
stack build
```

You can see the result [here](https://meta.plasm.us/).

