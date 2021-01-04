---
title: Lessons Learned From A Year Of Writing Haskell
header: Lessons Learned From A Year Of Writing Haskell
date: January 3, 2021
---

# The year in brief

* I spent the year as a software engineer writing Haskell, and switched to remote work during the pandemic with everyone else.    
* Wrote a lot of code, and focused on Haskell as a language for software engineering.    
* Completed 3 courses from Georgia Tech on statistics, computer networking, and embedded compilers, as part of a masters program I'll finish in 2021.    
* Wrote more Haskell code this year than probably all other years combined, and made it through 2020 feeling pretty good about things. I'll mark that in the win column!    

# This post
I'm writing this post as a collection of my thoughts on the last year, and a reflection on the major things I've learned.    
The majority of my Haskell experience is in the form of writing code for web servers that talk to databases serve up APIs, although I am very interested in compilers and language research!    

## Outline

* On The Practice Of Software Engineering
* Haskell Observations
* Musings On Type Level Programming
* Build Systems
* The Year In Readings
* Summary

## On The Practice of Software Engineering

Software engineering is about understanding the trade offs (performance, latency, domain modelling, et cetera), and implementing a solution that solves a problem with a solution space of development time, cost, and quality.
Not every project you engage in will focus on code quality, but working at a company where some projects can focus on quality is a tremendous joy and enriching experience, and am very thankful to my teammates who made this possible!    
    
One of the best exercises I had this year was going through my PRs and consolidating all the comments by theme, which taught me as much about my own code as what other people think about code in general.    
    
An important perspective when writing code, is understanding how that software will exist through time, what demands will be placed on it, and how the fundamental assumptions will inevitably change.
A big lesson for me this year is learning that a system can start out well, but through product mis-alignment, tight time constraints, and feature expansion, it can become an irreplaceable web of sadness littered through your code base too costly to remove!    
    
On a psychological note, through trial and error I realized I only have so many "high performance" hours in a week where you can be sustainably productive without lowering the quality of the hours you work.
Some corporations have even found workers are even more productive with 3 day weeks versus 5!
I'm not saying we should go that far, but when you're home all the time coding in a pandemic, balance is key to staying fresh and effective!    

## Haskell Observations

Contributing to ghc is HARD!    
It's a complex and old code base, and although there is plenty of documentation, it takes a ton of work to be able to understand ghc well enough to contribute at the level needed to fix bugs and implement new features.
That said, there is a ton of documentation work that still needs to be done, and is pretty beginner friendly!    

Use Template Haskell to generate code only as a last resort!
The available alternatives, including deriving instances, have grown rich with options over the past few years, and these alternatives are less likely to eat up your compiler time or make cross-compilation an herculean task!
Quasiquoters are a notable exception to this, as are inserting filenames/git commits at compile time, the key is to use it sparingly!    

[Haskell Planetarium](https://haskell.pl-a.net/) as a link aggregation has enough content to be readable everyday, and along with [Haskell Weekly](https://haskellweekly.news/) are my `goto` sources of Haskell related content.    
    
Next, and pretty obviously, **you can use Haskell to deliver value in business**.
This is already established, but watching different teams write similar web services at their direction showed me that there is a lot of variety within the ecosystem.    

Based on my experience, answers the following questions will vary from project to project, and asking them will get you oriented in a new project pretty quickly:    

* Which prelude is being used?    
* How are effects handled?    
* How are API routes defined?    
* How do you interact with the database and manage a transaction session?    
* How do you write an sql query?    
* What's the nature of the "expression problem" for internal data types?    
* What is the strategy for manipulating internal objects, i.e. lens/generic lens?    
* How are log messages generated?    
* What's the testing strategy, is a fuzziness generator used?    
* How do you run the tests locally, given the architecture?    

By the [Expression Problem](https://en.wikipedia.org/wiki/Expression_problem) I mean which typeclasses are needed to be derived or defined, like `ToJSON` or serialization.
I'd like to add that the overwhelming majority of effects I've seen have been handled using [mtl](https://hackage.haskell.org/package/mtl) and/or `IO`, but we appear to be close on extensible effects, and the word done on [eff](https://github.com/hasura/eff) looks promising!    

## Musings On Type Level Programming

A theme for this year for me was exploring the intersection between type level programming and good software engineering practices.
It's one thing to know Haskell, it's another to write beautiful code that's as easy to understand as it is to maintain.
As a prior, my stance on new, complex Haskell features is to "write junior code".
However, given a sufficiently experienced team familiar with type level programming, the trade-offs of the features themselves can be evaluated per se...    

### Haskell Type Level Solutions Worth Their Weight

* [squeal](https://hackage.haskell.org/package/squeal-postgresql), takes a while to learn, but with generated schemas helping write some code, a deep embedding tied to the schema is very maintainable. A key aspect of getting this solution to work is to have a repeatable way to generate the same schema that's used in production. Ours is ([squalgen](https://github.com/mwotton/squealgen)), which works great!     
* [generic-lens](https://hackage.haskell.org/package/generic-lens). For a long time, I avoided lens, but with `DuplicateRecordFields` and `OverloadedLabels` `generic-lens` provides some great utilities beyond lens/prism for manipulating structures that I've come to rely on.    
* [servant](https://hackage.haskell.org/package/servant) great for defining routes for clients/servers, and even the streaming stuff can work!    
* [DerivingVia](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html), very useful way to extend newtypes and generic deriving, and gives you a way to write a lot of maintainable code in a few short lines.   

### Haskell Type Level programming ideas I've unsuccessfully tried to apply, but am holding out hope for...

* **Type level witnesses**: although they did work for a specific problem, getting kind constraints to play nicely with associated libraries turned out harder than I imagined, and I'm not 100% convinced the added complexity if worth the effort.    
* **GADTs**: Know the type for each individual data constructor? Sounds good! I just haven't had a problem where this is exactly what I've needed.    
* **Existensial Types**: There is a bug that prevents generic deriving from "just working" reasonably in the last situation I tried them, and I'd rather just use an alternative approach, like a smart constructor, over writing my own generic instance. [GHC bug report](https://gitlab.haskell.org/ghc/ghc/-/issues/10514).     

## Build Systems
I'm not sure what else to say here, other than build systems in Haskell leave something to be desired!    
Stack works, but doesn't cache well, and there's the dreaded `flat namespace error` and other build errors that sometimes are most easily fixed by manually removing the offending library, or worst case removing your `.stack` directories.    
Alternatively, Nix is an efficient and robust build system, but is really complicated, and takes a non-trivial amount of time to learn and set up if you're going to use that for your CI build system.
I want to learn Nix and use it for personal projects, it's just a question of priorities, and figuring out how to get it to work with `ghcid`.    
In 2021, I should really try `cabal` :)    

### My development environment

The Haskell development environment has really improved over the last few years, and here's the setup I eventually settled on:    

* iTerm2 on MacOS, using `tmux` as a window/panel manager.    
* `neovim` as my version of `vim` for writing code.    
* `hasktags` to "jump to definition" with `vim` shortcuts. Not perfect with `OverladedLabels` and a large code base, but good enough!    
* `hoogle` command line utility to search for function types or package sources.    
* [Hackage](https://hackage.haskell.org/) online to browse docs. (press 's' on a package page to open a search prompt).    
* A variety of tools using `fzf` for searching within and for files, and browsing git diffs and commits.    
* [Serokell's Regex Hackage Search](https://hackage-search.serokell.io/) which just came out, but something I think could be very useful for arbitrary searches over Hackage!    
* `ghcid` is pretty amazing, and I'm not sure how I wrote Haskell without it! It gives fast incremental recompilation, and can be configured to run a test suite or bash command on completion.    

The haskell development environment is getting better all the time, and although I've set up Haskell Language Server a few times for small side projects, I haven't taken the time to try it on a larger code base, despite the extremely helpful project devs!    

My ideal tool, would be something that consolidates the different search interfaces (hackage, regex on hackage, local search, jump to definition, et cetera), to create a utility that jumps to definition or docs given what's in scope for a given module or even project.   


## The Year In Readings

Here's a sampling of the "essential" articles I came across in 2020.   

* [Haskell for a New Decade](https://www.stephendiehl.com/posts/decade.html) a good reading to start off the year!    
* [Lessons in Managing Haskell Memory](https://tech.channable.com/posts/2020-04-07-lessons-in-managing-haskell-memory.html) is a really interesting article about speeding up code execution time using "compact regions". Memory management in strict, typed functional programming languages is a pretty wide open area, "compact regions" are a worthy approach!    
* [Micro C](https://blog.josephmorag.com/posts/mcc0/) A Micro C compiler written in Haskell. A nice little compiler project!    
* [Type Witnesses In Haskell](https://serokell.io/blog/haskell-type-level-witness) Is a great write up by Sandeep Chandrika explaining type witnesses, and how to use them to ensure illegal state are not representable!    
* [GHC illustrated for hardware persons](http://takenobu-hs.github.io/downloads/haskell_ghc_illustrated.pdf) is an architectural diagram of the ghc backend, and includes information on exactly how ghc is able to run a lazy functional language on stock hardware. Useful if you are curious how certain things are implemented on the backend!    
* [Names are not type safety](https://lexi-lambda.github.io/blog/2020/11/01/names-are-not-type-safety/) is a great article by Alexis King on the practical benefits of using newtypes versus other representations to ensure correctness. Overall, I think this is a really thought provoking piece on how design influences type safety, and "just wrapping something in a newtype" can be underpowered.    
* [Mirror Mirror: Reflection and Encoding Via](https://www.parsonsmatt.org/2020/02/04/mirror_mirror.html) provides a walkthrough for how to implement a JSON encoder using deriving via, which is a great way to write encoders and decoders for various types!    

## With a little help from my friends!

Articles and projects by my co-workers...    

* [Adventures in Refactoring](https://samtay.github.io/posts/refactoring-adventures) A thoughtful walkthrough of refactoring a section of Haskell by Sam Tay, who uses a combination of techniques like LambdaCase and carefully analyzing errors to create very readable code.    
* [Enhancing Functor Structure Step-By-Step](https://blog.jle.im/entry/enhancing-functor-structures-step-by-step-1.html) Just Le walks through the "functor structure" programming style, using a specific example, and is a good introduction to his [Functor Combinatorpedia](https://blog.jle.im/entry/functor-combinatorpedia.html)    
* [When threadWaitRead Doesn't](http://jfischoff.github.io/blog/when-threadwaitread-doesnt.html) Is a deep dive into Haskell IO by Jonathan Fishoff, who tells the story of debugging a network connection issue.    
* [Commander-cli](https://github.com/SamuelSchlesinger/commander-cli) Sam Schleschesinger's type directed approach to parsing command line arguments.     
* [Capturing the magic of Prelude.interact](http://gelisam.blogspot.com/2020/12/capturing-magic-of-preludeinteract.html) Is an analysis of `Prelude.interact` by gelisam, with an eye towards the important interaction between pure and `IO` bound code.    
* [crdt-event-fold](https://hackage.haskell.org/package/crdt-event-fold-1.4.0.0/docs/Data-CRDT-EventFold.html) CRDTs are a fascinating idea for managing conflict-free operations over distributed systems, and Rick Owens provides a well engineered Haskell implementation with excellent docs!    

## In Summary

2020 was stressful year that forced us to question many of the systems and institutions around us, including the Haskell ecosystem, it's funding, and future direction. We celebrated the release of [ghc 9.0.1](https://www.haskell.org/ghc/blog/20201229-ghc-9.0.1-rc1-released.html) a major milestone, and look forward to the formation of the [Haskell Foundation](https://haskell.foundation/)    
It's my sincere hope the Haskell ecosystem can continue development through means that are parsimonious and beneficial for society.
Further, I hope 2021 is a year in which Haskell continues it's improvements as a stellar industrial programming language, and the lessons learned over it's first 30 years can be used to further programming language development for years to come!    
