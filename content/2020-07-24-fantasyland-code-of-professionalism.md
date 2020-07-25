+++
title = "The Fantasyland Code of Professionalism"
original_date = 2020-07-24T02:33:51
path = "posts/2020/07/24/fantasyland-code-of-professionalism"

[taxonomies]
tags = ["scala", "lambdaconf", "racism"]
+++

The [Fantasyland Code of Professionalism][fcop] (FCoP) is a code of conduct developed by the
[Fantasyland Institute of Learning][fiol], an organization that was founded by [John A. De Goes][jdg] and
is [responsible for][fiol-lc] [LambdaConf][lambdaconf], a functional programming conference.

Many other people have written about the shortcomings of the FCoP as a code of conduct, including [Christie Koehler][koehler],
who calls it "beyond mediocre" and "downright dangerous", and [Matthew Garrett][garrett] (in an article
titled "The Fantasyland Code of Professionalism is an abuser's fantasy").

The purpose of the post
you're reading now isn't exactly to critique the FCoP, though, but to preserve some of the discussion surrounding it,
since De Goes has recently deleted the [FCoP GitHub repository][fcop-github] and several other FCoP-related
documents, in a move that seems related to the fact that he's currently
[threatening to sue me for defamation][cease-and-desist].

One of the claims in [De Goes's cease and desist letter][cease-and-desist] is that the following statement (published [here][jdg]) is false:

> The FCoP was developed specifically in response to the 2016 LambdaConf controversy, 
> and it's clearly designed to protect white supremacists like Yarvin.

De Goes's lawyer [writes][cease-and-desist]:

> The FCoP is a code of conduct for professional communities that our client has created.
> The FCoP is clearly not designed to protect white supremacists.

I've provided evidence in [another document][response] that it's reasonable to describe Curtis
Yarvin as a white supremacist, and that many other people besides me have done this, including [journalists][baffler],
prominent software developers (for example [Erica Baker][baker] in [this Inc. article][inc]),
and [one of his former business partners][burnham].

<!-- more -->

## Yarvin as a white supremacist

I won't repeat all of that material here, but will include a couple of quotations from [Yarvin's blog][yarvin-1],
which [De Goes claimed to have reviewed][typelevel-email] before [he asserted][the-letter] that Yarvin "has never written any hate speech
or resorted to insulting or vulgar language":

> Not all humans are born the same, of course, and the innate character and intelligence of some is more suited to mastery than slavery. For others, it is more suited to slavery. … Thus, Spaniards and Englishmen in the Americas in the 17th and earlier centuries, whose sense of political correctness was negligible, found that Africans tended to make good slaves and Indians did not.

[And][yarvin-2]:

> If you ask me to condemn Anders Breivik, but adore Nelson Mandela, perhaps you have a mother you'd like to fuck.

Note that [Breivik][breivik] was a right-wing terrorist who killed 77 people in Norway in 2011 after publishing a racist manifesto.

## Deleted discussion threads

My assertion that the FCoP was "designed to protect white supremacists like Yarvin" was based in part on statements by
De Goes in GitHub issue threads on the FCoP repository. According to Google's search result cache, the repository was
deleted at some point after 23 May 2020, and it was not available during the week of 6 July 2020, when I received the
cease and desist letter.

I've currently recovered eighteen of these discussion threads from Google's cache and other archives, including the following:

* [Clarify "harm" in sabotage][issue-77]
* [FCOP doesn't no-platform white supremacists][issue-51]
* ["Moralizing" is a required element of enforcing any code of conduct][issue-54]
* [You can literally murder a member and rejoin the community 5 years later][issue-45]
* [Consider expanding scope of inactive participation violations][issue-43]

The other thirteen issues that I've been able to recover so far are available [here][issue-list].

In these discussions John De Goes [explicitly refused a request to ban white supremacists][issue-51],
using quotation marks for the term, making specific reference to Curtis Yarvin (to deny that he
is a white supremacist), and accusing the person making the request of lying:

> Second, if you were to create a COC that banned "white supremacists", it would have to allow CY
to participate, since he is not a white supremacist (quote or you're lying, it's that simple).

In [another thread][issue-77], De Goes hypothetically proposes a ban on Nazis and then explains why the FCoP would
not support it (note that he again refers specifically to Yarvin):

> These issues mean that, if you ban Nazis, people will use the No-Nazi clause to ban people they strongly disagree with, even if those people are not Nazis.
>
> Moreover, there is no even remotely plausible of "Nazi" in which, say, Curtis Yarvin is a Nazi. So if you ban Nazis, you'll still end up with lots of people like Curtis Yarvin, who believe that, while individual differences trump everything else, the mean of IQ test scores differs by "race".
>
> Ultimately, banning Nazis is not sufficient. In order to exclude precisely the people you want to exclude, your policy must be equivalent to, "We will exclude anyone we deem to be sufficiently bad or unwelcoming to those we wish to include." i.e. you base it on your subjective feelings about the person and your subjective estimation of what effect the person will have on those you wish to include (which feelings can be trivially manipulated by social media, by the way).

In the same thread, De Goes explicitly compares the FCoP to the [ACLU][aclu]'s defense of Nazis:

> Do you understand why the ACLU defends the rights of Nazis to speak? It's not because they endorse the views of Nazis; rather, they find them repugnant. It's because if Nazis are denied the right to speak, then there will be another group which follows them (to say nothing of the warfare of disingenuously casting one's opponents as "literal Nazis", as has been done with Charles Murray and many others, for example); and so on, until what one is allowed to speak about and how one must speak about it is determined entirely by a fickle authoritarian government.
>
> What you want cannot be satisfied by FCOP.

In response to a hypothetical question about whether the FCoP would condemn a community member who caused another member
to be fired for being a [Ku Klux Klan][kkk] leader, De Goes said it potentially would not, assuming that the Klan member's
participation in a racist organization didn't affect the performance of their professional duties:

> This depends on whether or not being a "KKK leader" has implications on the performance of the professional duties of A, which it arguably could have depending on specifics.

## Typelevel

De Goes made similar arguments in [a thread][typelevel-comment] where he proposed the FCoP for approval by the
[Typelevel organization][typelevel], where he also echoes transphobic rhetoric about "4 year olds transitioning", mocks
"the unbounded growth of gender pronouns", and defends advocates of scientific racism:

> As far as I am aware, there are exactly zero Scala programmers in the world who vilify Jews, promote national socialism, or engage in rape threats to women (all of which are illegal somewhere, and most of which are illegal everywhere, and are already extensively covered by FCOP).
> 
> Rather, all the real world cases are programmers being excluded for being conservative (including being banned from multiple tech communities), programmers being excluded for being Gorean, programmers being persecuted for questioning whether 4 year olds should transition, and similar.
> 
> These are the actual issues facing programmers, not the imaginary ones that you list.
>
> Any community could layer on restrictions on participation in FCOP; they just have to be transparent about it, which is all that is being asked for by concerned parties. Want to ban nazis, slavery apologists, and so forth? Fine, then just be explicit about it, rather than hiding behind a vague COC.
>
> These restrictions need to be clear on what they mean. Goreans have been accused of being "misogynists" and "slavery apologists", though everything in Gor culture is consensual. Neo-reactionaries have been accused of being "Nazis". People who have expressed concerns about the unbounded growth of gender pronouns have been accused of denying basic human rights to non-binary gendered individuals. People who believe there might be small differences in the distribution of IQ across sex or "race" have been labeled misogynists or racists.
>
> Don't expect everyone to agree on these issues, because they won't.

Note that unlike almost everything else quoted in this post, this comment hasn't been deleted,
probably only because it's in a locked thread of a repository that De Goes
does not control.

Note also that the Typelevel leadership allowed this issue to remain unresolved
for almost nine months, despite De Goes's transphobic and otherwise abusive rhetoric, and that they
never made a formal response, despite [Miles Sabin's assertion][sabin] that they would.

## Statement of purpose

The Fantasyland Institute of Learning's [statement of purpose][fiol-purpose] (now deleted)
also explicitly states that the FIoL and FCoP were developed in response to the controversy surrounding
Yarvin's invitation to LambdaConf 2016:

> The LambdaConf Controversy
>
> Our current views on this issue emerged from an incident in early 2016.
> 
> In April 2016, FIOL’s own LambdaConf event was attacked for refusing to ban a speaker with unpopular political views, whose anonymized proposal had been accepted by a blind committee. Activists attempted to shut down the conference by pressuring speakers and sponsors to withdraw, organizing protest petitions, and otherwise working behind the scenes to harm attendance.
> 
> Until the LambdaConf controversy, most conferences attacked for similar reasons were either destroyed (due to insufficient funds) or they caved to demands made on social media. LambdaConf staunchly refused to alter its position and was prepared to accept any consequences of this decision.
> 
> With the help of Status 451 (a free speech advocacy blog), a fundraiser was launched to support LambdaConf. LambdaConf ended up raising $40,000 with this campaign. Dozens of speakers volunteered to take the place of those who backed out, and new sponsors came in to help cover the costs for the event.
> 
> Thanks to all the support, the conference sold out beyond capacity, the event was a tremendous success, and FIOL ended up more financially secure than ever before.
>
> After successfully weathering the LambdaConf controversy, we at FIOL decided that we would use our experience to support other organizations who wish to take a stand in refusing to impose ideological purity tests on speakers and attendees.

## The deleted repository

Lastly, while De Goes has deleted the FCoP GitHub repository, it was published under a
Creative Commons license (specifically [CC BY-ND 4.0][cc-by-nd]), so it can be freely republished
(without modifications). I don't want to give the project uncontextualized visibility by sharing it
on GitHub, but I've published a [Git bundle][git-bundle] archive [here][fcop-bundle]. If you
download the bundle, you can open it with `git clone fcop.bundle`.

Note that I was only able to find the history up until February 2018 (commit `876a618b`), and that
the bundle only includes the master branch. If anyone has a checkout with more recent commits or
other branches, please contact me.


[aclu]: https://www.aclu.org/
[baffler]: https://thebaffler.com/latest/mouthbreathing-machiavellis
[baker]: https://twitter.com/EricaJoy
[breivik]: https://en.wikipedia.org/wiki/Anders_Behring_Breivik
[burnham]: https://twitter.com/John_C_Burnham/status/1285828693567381504
[cc-by-nd]: https://creativecommons.org/licenses/by-nd/4.0/legalcode
[cease-and-desist]: https://gist.github.com/travisbrown/5fb741a2619c86aed0d7f1838aa5e33d
[fate]: https://web.archive.org/web/20170329164815/http://fantasyland.institute/initiatives/FATE.html
[fcop]: http://fantasyland.institute/initiatives/COC.html
[fcop-bundle]: https://meta.plasm.us/archive/fcop/fcop.bundle
[fcop-github]: https://github.com/fantasylandinst/fcop/
[fiol]: http://fantasyland.institute/
[fiol-lc]: https://tweetstamp.org/740307713805029377
[fiol-purpose]: https://web.archive.org/web/20170329164815/http://fantasyland.institute/initiatives/FATE.html
[garrett]: https://mjg59.dreamwidth.org/46791.html
[git-bundle]: https://git-scm.com/docs/git-bundle
[inc]: https://www.inc.com/tess-townsend/indiegogo-campaign-funding-tech-conference-white-nationalist.html
[issue-43]: https://meta.plasm.us/archive/fcop/43.html
[issue-45]: https://meta.plasm.us/archive/fcop/45.html
[issue-51]: https://meta.plasm.us/archive/fcop/51.html
[issue-54]: https://meta.plasm.us/archive/fcop/54.html
[issue-77]: https://meta.plasm.us/archive/fcop/77.html
[issue-list]: https://meta.plasm.us/archive/fcop/
[jdg]: https://meta.plasm.us/posts/2019/09/01/jdg-and-the-fp-community/
[kkk]: https://en.wikipedia.org/wiki/Ku_Klux_Klan
[koehler]: https://subfictional.com/an-analysis-of-the-fcop/
[lambdaconf]: https://lambdaconf.us/
[response]: https://meta.plasm.us/posts/2020/07/25/response-to-john-de-goes/
[sabin]: https://github.com/typelevel/general/issues/74#issuecomment-303031058
[the-letter]: https://amar47shah.github.io/posts/2016-03-28-lambdaconf-yarvin.html
[typelevel]: https://typelevel.org/
[typelevel-comment]: https://github.com/typelevel/general/issues/74#issuecomment-302972140
[typelevel-email]: https://gist.github.com/travisbrown/a542b7eaa8fe937f99877522d5e46edd
[yarvin-1]: https://web.archive.org/web/20200629084814/https://www.unqualified-reservations.org/2009/07/why-carlyle-matters/
[yarvin-2]: https://web.archive.org/web/20200629082346/https://www.unqualified-reservations.org/2011/07/right-wing-terrorism-as-folk-activism/
