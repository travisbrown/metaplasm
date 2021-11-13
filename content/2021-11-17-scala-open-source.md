+++
title = "A statement about my Scala open source work"
original_date = 2021-11-17T04:20:34
path = "posts/2021/11/17/scala-open-source"
description = "This document describes my decision to stop doing open source work in the Scala community."

[taxonomies]
tags = ["scala"]
+++

This post is about open source burnout, but it's not about adopters feeling entitled to my time, or
disagreements between maintainers, or anything like that. My personal experience of working on open
source Scala projects for the past decade has been almost entirely positive, with only one or two
exceptions that I can think of off the top of my head, out of dozens of thousands of interactions.

Instead this post is about my unwillingness to continue contributing my time to a community whose
leadership is characterized by unreflective privilege and petty vindictiveness (Martin Odersky),
grifty amoral opportunism (John De Goes), cowardice and inaction (Typelevel), and betrayal of trust
(the Scala Center).

<!-- more -->

I'll start by introducing myself. Much of my career for the last decade has involved working in the [Scala programming language](https://www.scala-lang.org/). This has included several threads of open source and community work:

* I created and maintain a [family of libraries](https://github.com/circe) for working with JSON and related formats in Scala.
These libraries are used by hundreds of companies, and their artifacts are currently downloaded from [Maven Central](https://maven.apache.org/repository/) around 7.5 million times per month.
* I am the top overall contributor to [Cats](https://github.com/typelevel/cats),
a popular library for functional programming in Scala that is published by [Typelevel](https://typelevel.org/),
an organization that I formerly helped to moderate (but chose to separate myself from [in 2019](https://github.com/typelevel/typelevel.github.com/pull/244), specifically due to the failure of the Typelevel leadership to take a stand against bigotry expressed in [this Typelevel thread](https://github.com/typelevel/general/issues/74)).
* I published all Cats releases in the early 2.x release series, culminating in [2.2.0](https://github.com/typelevel/cats/releases/tag/v2.2.0),
which introduced several major improvements to the library.
* I've also helped to create, maintain, document, and popularize dozens of other Scala libraries,
including an [implementation of the Dhall configuration language](https://github.com/travisbrown/dhallj), an [iteratee implementation](https://github.com/travisbrown/iteratee), and a library for [type-class-related code generation](https://github.com/typelevel/simulacrum-scalafix).
* In late 2019 and early 2020 I was an early adopter of Scala 3 (while it was still called "Dotty"), and [helped to drive](https://github.com/typelevel/cats/pull/3269) the Scala 3 migration for Cats and many other Scala libraries.
Over the few months that I was using Dotty, I filed [28 compiler bug reports](https://github.com/issues?q=is%3Aissue+author%3Atravisbrown+archived%3Afalse+user%3Alampepfl+is%3Aclosed), fixed a [couple of compiler bugs](https://github.com/issues?q=is%3Apr+author%3Atravisbrown+archived%3Afalse+user%3Alampepfl+is%3Aclosed), and contributed an [implementation](https://github.com/lampepfl/dotty/pull/7775) of backward-compatible support for a popular Scala 2 compiler plugin.
* I am the third top-ranked contributor in the [Scala tag on Stack Overflow](https://stackoverflow.com/tags/scala/topusers), where I've [answered 1,270 questions](https://stackoverflow.com/search?q=user:334519%20[scala]%20is:answer) over the last decade.

All of this prelude is just to say that I've invested a significant part of my career in the Scala programming language, that I've cared deeply about its adoption in the past, and that I still care deeply about the culture of the Scala community.

As of today, though, I'm stepping away from all of these projects.
I won't be reviewing pull requests, fixing bugs, answering user questions, or publishing new releases.
The only exception is that in the short term I will continue to moderate venues I personally maintain,
which currently primarily means the [Circe Gitter channel](https://gitter.im/circe/circe).

I have no objection to other people continuing work on these projects,
and if anyone wants to maintain any of them, I'm happy to discuss passing
along GitHub and Maven Central access.

I've not made this decision lightly.
I've watched friends and colleagues walk away from the Scala community in disgust since at least 2014,
and I've spent years trying to call attention to the patterns of behavior that caused them to leave.

In the past I had thought that the problems of the community were primarily the work of a few bad actors:
[John De Goes](https://meta.plasm.us/posts/2019/09/01/jdg-and-the-fp-community/),
[Jon Pretty](https://yifanxing.medium.com/my-experience-with-sexual-harassment-in-the-scala-community-9245b4a139de),
[Tony Morris](https://twitter.com/travisbrown/status/1100795294160248832),
[Paul Phillips](https://gist.github.com/travisbrown/d6bb40b8feb39fbf9b60fa0d3352c0ee),
etc. I no longer think that this is the case, for several reasons. Instead I've come to believe that the
problems have always been the result of failures of leadership, and that those failures have been getting worse, or at least
more publicly visible.

The most significant of these failures has been the [Scala Center at EPFL](https://scala.epfl.ch)'s
catastrophic response to reports of sexual harassment by one of its founding officers, which first
became publicly known earlier this year. This response included a cover-up
and an [explicit refusal](https://www.scala-lang.org/blog/2021/05/11/scala-center-action-towards-safe-respectful-env.html)
to condemn the abuser's actions in the center's official public statement on the matter.

Another development is that [Martin Odersky](https://en.wikipedia.org/wiki/Martin_Odersky), the creator of Scala and [Academic Director of the Scala Center](https://scala.epfl.ch/team.html), has recently made his positions on some matters of community leadership much clearer than they'd been in the past. [This comment](https://github.com/scala/scala-lang/pull/1088) is one early example. His threats in [this GitHub discussion](https://github.com/tpolecat/doobie/pull/1587#issuecomment-961415475) and [this Scala Contributors thread](https://contributors.scala-lang.org/t/politics-safety-and-the-future-of-scala/5317) are others.

Odersky has emailed my colleagues to ask them to "disavow" me because I liked a tweet criticizing him.
He ranted at Typelevel's leadership because they dared to [ban John De Goes](https://typelevel.org/blog/2019/09/05/jdg.html) on his (Martin's) birthday.
This kind of pettiness and vindictiveness is dangerous in a person in Odersky's position of power.

Lastly I've come to lose any confidence I still had in the ability of the Typelevel organization to act
as a counterweight to these problems. The Typelevel leadership has sometimes muddled their way into
taking the right actions, but usually too late and for the wrong reasons. One example is the
statement about their decision to ban De Goes, which completely ignored [dozens](https://meta.plasm.us/posts/2020/07/25/response-to-john-de-goes/)
of reports from people who had been targeted by De Goes, and focused instead on his "combative style" in technical discussions.
I personally believe that this explanation was at least partly disingenuous, and in any case it has
provided years of ammunition to bad-faith detractors.

There are many other examples of this kind of thing, including the [Typelevel issue](https://github.com/typelevel/general/issues/74) linked above,
where comments repeating racist and transphobic conspiracy theories
(e.g. "programmers being persecuted for questioning whether 4 year olds should transition")
were allowed to stand unaddressed for months, despite many people (including me) asking the
Typelevel leadership to provide a clear response.

Several of the members of the Typelevel Steering Committee are friends or former colleagues of mine,
and I have deep respect for a few of them, but I don't believe that Typelevel as an organization
will ever be able to break out of this cycle of accommodation, delay, and ineffective action.

I'm not sure what it would take for me to reconsider this decision. NthPortal's comment [here](https://contributors.scala-lang.org/t/politics-safety-and-the-future-of-scala/5317/56)
would be a good start. I would also want to see the directors of the Scala Center step down and publicly
acknowledge the harm that they've done. I don't expect any of these things to happen.

I'll probably continue writing some Scala occasionally, at least until I can find a better way to do one-off
data munging tasks with types in a REPL.
I also don't intend to stop trying to raise awareness about the problems in the community
(sorry to disappoint anyone who's read this far and has been hoping for that). If anyone has questions about this statement,
or if you want to adopt any of my Scala projects, I'm easy to find.
