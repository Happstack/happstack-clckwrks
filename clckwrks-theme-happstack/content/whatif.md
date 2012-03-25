What if web programming was fun and beautiful?

The goal of Happstack is to fully exploit the unique properties of Haskell
to create a web framework that is *better* than what could be done in
any other language.

Simon Peyton-Jones once described Haskell as "the world's finest
imperative programming language". And, in that spirit, we could easily
implement a typical, imperative-style web framework in Haskell. But,
then we could only expect to get typical results.

There are five traits that all web frameworks strive to have:

 - Correctness/Robustness
 - Easy to understand and reason about
 - Expressiveness/Conciseness
 - Managable Growth
 - Performance/Scalability

We believe that the way we use Haskell gives us a significant
advantage in each of these areas.

Correctness
===========

Errors Eliminated By Using Haskell
----------------------------------

Many common errors are eliminated in Happstack simply because it is
written in Haskell. And, unlike unit tests, these benefits
automatically extend to user code as well.

It would be futile to make Happstack perfect, if apps developed with
it are constantly riddled with bugs. The code unique to a web
application may be many times larger than Happstack itself. So, it is
of critical important to pick a language which makes it easier for
developers to write correct code.

Experience Haskell developers will already be familiar with the types
of errors that Haskell eliminates. But, in case you are new to
Haskell, I would like to highlight a few key instances.

A majority of modern web frameworks are written in garbage collected
languages such as Haskell, PHP, Java, Python, Ruby, Javascript,
etc. Happstack is no exception. Using a garbage collected language
eliminates many common errors that can lead to server crashes and
security issues. This includes errors such as buffer overruns and
invalid pointer dereferences which are often used as starting points
for server exploits.

Haskell goes one step further by eliminating errors caused by using
uninitialized variables. For example, we can not write:

> let x = ...

with out specifying what value `x` should be bound to. If we try
to use `x` before binding it to a value, we will always get a compile
time error.

Because Haskell is a statically typed language, many other common
errors such as calling functions with the wrong number of arguments,
arguments of the wrong type, or the wrong order can be eliminated at
compile-time.

It is quick and easy to define new types in Haskell. As a result, we
can create meaningful types that reduce the change of making a
mistake. For example, instead of having a function like this:

 > serveDirectory :: Bool -> FilePath -> ServerPart Response

we can write:

> data DirectoryBrowsing = EnableBrowsing | DisableBrowsing
>
> serveDirectory :: DirectoryBrowsing -> FilePath -> ServerPart Response

In the first example, it is unclear what the `Bool` is supposed to
represent. Hence, it is easy to remember wrongly what it is supposed
to do, or to forget what result `True` and `False` correspond
to. Thus, it is necessary to constantly refer to the documentation to
remember proper usage.

In the second example, the more meaningful name makes it far more
intuitive what the argument represents and the developer is unlikely
get the wrong polarity.

Types and functions can be used to create EDSLs
(embedded domain specific languages). In languages like PHP,
developers often resort to creating SQL queries, HTML, or javascript
by string concatenation. If the developer is not 100% accurate in
ensuring that user supplied data is properly escaped, it can lead to
[code injection](http://en.wikipedia.org/wiki/Code_injection) attacks.

In Happstack we embrace EDSL solutions which automatically ensure that strings are escaped properly. Consider the following HSX example:

> foo :: String -> XMLGenT Identity XML
> foo str = <div><% str %></div>

If `foo` is called with an String that contains reserved characters:

> foo "<b>"

it will automatically be rendered as:

<pre>
<div>&lt;b&gt;</div>
</pre>

<h3>type-safe URLs</h3>

Type-safe URLs are great example of leveraging the type system for extra safety. The idea is to use algerbraic data types instead of strings to represent URLs in the code. For example, the routes:

> /home
> /books
> /book/<int>

could be represented by the type:

> data Route =
>       Home
>     | Books
>     | Book BookId

This helps to eliminate a whole bunch of errors. A simple typo like `Hmoe` instead of `Home` will now be caught by the compiler. It is not possible to create a link to the `Book` with out supplying a `BookId`. And, by using `BookId` instead of `Int`, it is clear what the value represents. It also makes the code more robust to change. If the `Books` constructor is later removed, the compiler will automatically find any places that still try to link to it. type-safe URLs were originally pioneered by Happstack, but are such an attractive idea that they have been idependently reinvented a number of times since then. For more information see the [web-routes](http://happstack.com/docs/crashcourse/WebRoutes.html) section of the Happstack Crash Course.

Aside from providing addition safety, type-safe URLS via `web-routes` form the basis a composable web plugins. Each component needs to use unique URLs so that two plugins do not try to handle the same route. Since the plugins use types internally instead of strings, the plugin developer does not have to concern themselves with the prefix. Instead the unique prefix can be added where the plugin is registered with the parent application.

Better Testing
--------------

To show that `happstack-server` is working correctly requires proof
beyond "It works for me!".

Haskell provides a number of tools which are used to provide this proof.

<h3>Type Checker</h3>

The fact that Happstack compiles provides some assurance that it works correctly. The type checker is essential a tool that creates a proofs that the code is free of many common errors. It is not uncommon to find that once code is finally accepted by type checker that it works correctly on the first run.

<h3>Unit tests, QuickCheck, HUnit, and HPC</h3>

Some errors go beyond what the type-checker can catch. For example,
the type checker is not able to confirm that a parser conforms to the
specification. That requires unit tests.

Haskell, has a number of advantages when it comes to writing unit tests. Many functions in Happstack are pure (also known as referentially transparent). That means the functions do not have the ability to perform IO, access global state, etc. The output of the function is determined entirely by the inputs to the function.

This makes it easier to write unit tests because there is no need to setup a complicated environment to test a function or worry about how the function would behave different if the environment was different. Reducing the number of variables that can change reduces the number of tests that need to be created and run.

A combination of purity and expressive types in Haskell have lead to a unique and powerful testing library named QuickCheck. Briefly, QuickCheck is a two step process. The first step is to define properties that the functions must honor. These properties are written as Haskell functions. For example, if you reverse a finite list twice you get the original list:

> reverse_prop :: [a] -> Bool
> reverse_prop list = list == (reverse (reverse list))

Then QuickCheck is used to create a bunch of random test data values and check that the properties hold.

The HUnit library is used to create typical unit tests where the developer supplies a test case and the expected answer.

GHC includes the Haskell Program Coverage (HPC) tool. [HPC](http://www.haskell.org/haskellwiki/Haskell_program_coverage). HPC adds instrumentation to the executable which records what code paths are executed at run time. HPC is typically used to measure how effectively the unit tests cover the code base.

<h3>Grammar Driven Testing</h3>

A top priority for current Happstack development is improving coverage and accuracy of the unit testing. One idea we are looking to exploit is grammar driven testing. HTTP is full of RFCs specifying how to parse requests and format responses. But, writing unit tests for these grammars is an error prone process in itself. There are two key problems.

First, the developer may misunderstand what the grammar specification says. That misunderstanding is then translated into code and the corresponding unit tests. As a result, while the unit tests confirm that the implementation does what the developer intended -- the code is still doing the wrong thing.

Secondly, using HPC can confirm that the test code has 100% coverage. But that show that all valid inputs are handled correctly. Consider the following `reverse` function and test:

> reverse :: [a] -> [a]
> reverse _ = []
>
> reverse_test :: Test
> reverse_test =
>       "reverse_test" ~: ([] @=? reverse [])

In this example, the test suite will succeed, and HPC will show 100% test coverage, yet the reverse function is almost completely broken, working correctly for only one possible input.

We hope to address both these issues through grammar driven testing. The correct behavior for http parsers and formatters is largely specified in the the RFCs using EBNF notation. If we can translate these EBNF grammars into Haskell in a fairly mechanical manner, then we can use them to automatically generate a bunch of test data that represents a much wider coverage of possible valid inputs.

To address the first problem, we need to be sure that the process of translating the grammar from the RFCs to Haskell happens in a very mechanical manner that does not require interpretation of the meaning. For example, we might copy the grammars directly from the RFCs and parse them. Or we could use a DSL that closely mirrors the original syntax, so that a simple visual inspection gives a high level of confidence that the translation is correct.

<h3>Real-World Testing</h3>

HTTP clients do not, in general, adhere strictly to the specifications. Clients often support special extensions, or just simply implement the specifications incorrectly. Happstack has been around over 7 years, and so the code also includes a number of modifications needed to make the server practical for the real world.

As part the effort to implement grammar-based testing, we also plan to better document where and why Happstack deviates from the specifications.

Easy to Learn, Use, and Understand
==================================

There are a number of factors that go into making Happstack easy to learn, understand, and use, including documentation and design choices.

Documentation
-------------

Great documentation is essential for creating an easy to use and understand framework. Even if no one ever read the documention, it would help -- as writing documentation forces developers to see their code in a different light and often results in improvements to the API and bug fixes.

One key to great documentation is making sure that the right type of documentation is available to developer when they need it.

Happstack provides several types of documentation:

 - a short, getting started guide (less than 2000 words) intended to help get the developer writing simple web applications quickly as possible

 - the API contains extensive Haddock documentation

 - The Happstack Crash Course offers extensive goal oriented documentation with many self-contained, runnable examples

 - there are also several larger examples such as [happstack-imageboard](http://src.seereason.com/examples/happstack-imageboard/)

The next major documentation process will be a book showing how to develop a real world web application start-to-finish. This includes information on how to collect specifications, how to organize your modules, and other big picture concepts and best practices that are not covered in the more immediate reference material such as the API docs and the Crash Course.

False Simplicity
----------------

Sometimes simplicity and ease of use can be at odds with each other. For example, an API with a fewer functions can be easier to learn. Having fewer choices means fewer things to learn and fewer things to remember. But, a smaller API does not always translate to an easier to use system. For example, imagine if Happstack contained a only single function for looking up values from query parameters, form submissions and cookies:

> getParam :: Text -> ServerPart (Maybe Text)

Having a single call with a simple type is certainly easy to learn. But, that initial simplicity can result in code that is more complex, more error prone, and more difficult to understand. For example, the `getParam` function does not provide any way to specify that the parameter must come from the query string, nor does it provide any simple way to parse the value to another type, and it does not provide anyway to report errors.

The Happstack solution to this problem is multifold.

Start with the Simple Stuff
---------------------------

It is essential to not overwhelm developers who are just starting to use Happstack. That is one place where a small and simple API can be valuable. But, crippling Happstack for the sake of beginners would be a big mistake. Instead, we provide the `happstack-lite` package and a short getting started tutorial that covers all the essentials needed to start developing web applications. `happstack-lite` contains the essential subset of `happstack-server` that is required to develop web applications. This allows developers to focus on the really important parts first with out getting overwhelmed by functionality they are not ready to use yet.

`happstack-lite` aims to be as simple as possible, while still being practical. Most web applications could be written using `happstack-lite` instead of `happstack-server`. All the functions and types are included in a single, small module. Confusing things like class constraints and monad transformers are, for the most part, nowhere to be seen. `happstack-lite` tries to stick to one-way to solve any particular problem, so that the developer is not asked to decide between multiple options that they are not yet qualified to understand.

`happstack-lite` is implemented by re-exporting functions from `happstack-server` with easier to understand type signatures. As a result, it is fully compatible with the full-fledge `happstack-server`. This means that developers can start with `happstack-lite` but import additional functionality from `happstack-server` over time as they become more adept. The Happstack Crash Course is designed to help with this process. The Crash Course is divided into a bunch of small, problem-oriented sections that can largely be understood independently. This allows developers to read the sections in whatever order they find they need the information.

Reuse Existing Classes
----------------------

One reason why Haskell classes are powerful is that they tend to represent useful abstractions that follow certain laws. Many of these abstractions are well-grounded in a solid mathematical foundation. Once a developer understands the abstraction and the laws it follows, they are able to predict fairly easily how class instance for a specific type will behave.

By leveraging abstractions that Haskell developers are already familiar with, we can reduce the number of new things that they must learn. Accordingly, much of Happstack functionality is based around familiar classes such as Functors, Applicative, Alternative, Category, Monad, MonadPlus, MonadReader, MonadState, etc.

The new [pipes](http://hackage.haskell.org/package/pipes) library is very appealing to us because it promises to provide the resource finalization and exception handling of iteratees and conduits in a simpler package that leverages familiar abstractions. The pipes author, Gabriel Gonzalez, has focused very careful on getting the core abstractions correct. He has been building on a solid mathematical foundation of Category, Monad, generalized Arrows, etc, and making sure that the code obeys all the laws. The end result [appears to be](http://www.reddit.com/r/haskell/comments/rbgvz/conduits_vs_pipes_using_void_as_an_input_or/) a library which can do all the things that conduit can with fewer types and functions, and more reuse of existing classes and functions. Though it is still too early to say for sure.

Expressiveness/Conciseness
=========================

Another place we maintain careful balance is in using combinators, template haskell, and quasiquotes.

Combinators
-----------

Combinator based DSLs are great because of their expressiveness and extensibility. Accordingly, we strongly prefer combinators when possible.

For example, many web frameworks (Haskell and non-Haskell alike) define some sort of special syntax for routing urls to handlers. These route mappings are often represented as a string with some special notation for variable captures. In some cases the string is parsed at runtime, in other cases it is parsed at compile time by some preprocessor.

The advantage of that method is that it can provide a very concise and readable syntax for parsing simple routes. Additionally, a compile time preprocessor can perform routing optimizations and checks for errors. But there are also several drawbacks. For example, if the string as parsed at runtime, then errors won't be caught until runtime.

More importantly, though, is that such systems tend to lack expressiveness and extensibility. To the degree that extensibility is permitted, it often requires that the developer use a second, completely different parsing system.

In Happstack, we use boomerang to provide a routing DSL that is easy to read, and easy to extend. For example, we could parse a route like:

>    "/article/123"

using a combinator like:

> "article" </> int

Instead of returning an `Int` we might want to return an `ArticleId`. That can be done by defining a new combinator:

> articleId :: Router ArticleId
> articleId = xmap ArticleId (Just . unArticleId) int

and rewrite the code to:

> "article" </> articleId

More information on boomerang and web-routes can be found [here](http://happstack.com/docs/crashcourse/WebRoutes.html#web-routes-boomerang).

Happstack 7 has some simple, path segment oriented routing combinators, such as a `dir` and `path`. In Happstack 8 we will look at bringing more powerful `boomerang` style combinators to the path segment oriented routing combinators as well.

Template Haskell
----------------

In Happstack, we try to restrict the use of Template Haskell to places where the code generation is very mechanical. For example, in the same way that it is possible to derive classes like Eq, Ord, Show, and Read, it would be really nice to be able to derive SafeCopy instances. Since there is no way to extend the `deriving` clause in Haskell, we resort to some Template Haskell code which has the same effect.

QuasiQuotes
-----------

In Happstack, we prefer to restrict the use of QuasiQuotes to
embedding pre-existing syntaxes and grammars into Haskell. For
example, the preferred solution for embedding javascript in Happstack
is the
[jmacro](http://happstack.com/docs/crashcourse/Templates.html#jmacro)
library.

`jmacro` allows developers to write javascript code using the syntax they are already familiar with. Creating a DSL using combinators would hurt far more than it helps. Developers would spend a lot of time thinking about what they want to write in javascript, and then trying to figure out how to translate that into combinators, etc.

Instead, `jmacro` allows the developers to stick with the familiar syntax they already know, while still being able to leverage code generation and safely splice Haskell types into javascript code. As an added benefit, `jmacro` also provides compile-time syntax checking, the ability to generate unique names for global variables/functions, and an experimental type checker.


Performance and Scalability
===========================

The Glourious Haskell Compiler (GHC), produces relatively fast, compiled code. As shown in [Computer Language Benchmarks Game](http://shootout.alioth.debian.org/u32/which-programming-languages-are-fastest.php), compiled Haskell code is only 2-3 times slower than C and Fortran, and 10x faster than languages like PHP, Ruby, and Python.

The GHC runtime system (RTS) provides exceptionally fast and scalable concurrency. Haskell applications are able to easily scale to millions of threads and have very fast interthread communication. Haskell tops the list in [this threading benchmark](http://shootout.alioth.debian.org/u32/benchmark.php?test=threadring&lang=all).

The GHC portable I/O event manager was designed from the ground up to support high-concurrency network servers, supporting millions of concurrent network connections with millions of active timeouts. It is based on modern event notification facilities like epoll instead of select.

Haskell's commitment to purity and static typing has allowed for the development of easy to use concurrency and communication mechanisms, such as [software transactional memory](http://www.haskell.org/haskellwiki/Software_transactional_memory) (STM). STM allows for composable and modular conconcurrent code, ensuring that deadlocks do not arise -- something that most other concurrency mechanisms can not guarantee.

`happstack-server` is already significantly faster than popular frameworks written on Python and Ruby, able to serve tens of thousands of requests per second. In Happstack 8 we expecting to get a 4 fold increase in speed, making `happstack-server` one of the fastest HTTP servers in the world.

Maintainable
============

Haskell code tends to have good long term maintainablity. There are a few contributing factors.

 - Haskell is a fairly expressive language, allow ideas to be expressed clearly and with fewer tokens. This makes it easier to read and understand code. This readability becomes especially important when new developers are added to the project, or when developers have to visit old code.

- Haskell is a rather flat language consisting mostly of functions and algebraic data types. Because many functions are pure, there is no global environment or state to worry about. This makes it easy to reason about a function's behaviour in isolation. Haskell code tends to get large and unweildy at a much slower rate than many other languages.

 - Refactoring is easy because the type-checker will automatically find all the places that are affected by changing data type declarations, function type signatures, and function names. It is not uncommon to significantly refactor code, and find that after spending a few hours fixing all the compiler errors, the refactored code works correctly on the first run.

Sometimes it is necessary to make breaking changes to Happstack in the name of progress. When this happens, we always provide clear guidelines on how to migrate your code.

Flexibility
===========

Happstack is designed to be flexible, giving the developers the freedom and flexibility to choose the technologies that best for the unique problems they are facing. Hence we try to provide a wide variety of options for templating, routing, persistent storage (aka, database), etc. We try to avoid a one size fits all mentality.

Pushing the Boundaries
======================

[acid-state](http://happstack.com/docs/crashcourse/AcidState.html) (formerly known as `happstack-state`) is a superb example of the Happstack philosophy in action. `acid-state` is a high performance library for adding ACID guarantees to (nearly) any kind of Haskell data structure. In layman's term, it's a library that will make your data persist even in the event of, say, a power outage. Unplug your machine and restart and have your app recover to exactly where it left off. `acid-state` spares you the need to deal with all the marshalling, consistency, and configuration headache that you would have if you used an external DBMS for this purpose.

`acid-state` works directly with pure, expressive Haskell algebraic data types instead of a tiny, restrictive set of primitives like integers and string. This allows developers to create expressive, meaningful types. Additionally, developers do not have to worry about marshalling their data back and forth between Haskell and some other external representation, nor do they need to keep two different mental models of the data in their head.

Queries and updates are written as pure functions in the `Reader` and `State` monads. This means developers do not need to learn a new language with a complete different programming paradigm just to get ACID guarantees. Additionally, they are not required to force their design into a relational model when all they really want is ACID. Though, they can still use a relational model on top of `acid-state` when appropriate. The developer is able to retain all the benefits of Haskell coming from purity, type-checking, type-classes, polymorphism, code reuse, etc. In fact, `acid-state` is only possible *because* of the Haskell type system and purity.

`acid-state` is also impervious to SQL-injection style attacks. Haskell SQL DSLs will generally prevent inject attacks by makes sure user submitted data is properly escaped. But with `acid-state` there is simply no way that data submitted by a user could every be mistaken for code that should be run.

Because `acid-state` events are pure and written in Haskell, it is easy to unit test them in isolation using standard tools like HUnit and QuickCheck. `acid-state` also offers special memory based backend that stores all the transaction logs in RAM. It's intended use is to make it easier to write tests that run against a populated `AcidState` by not requiring the tests to create and files on disk or clean them up afterwards.

`acid-state` also provides built-in mechanisms for safe data migration, making it easier to push new versions of the server live.

`acid-state` is a library which is linked directly in the end application. This makes it easier to deploy and application because there is no extra server that needs to be installed, configured and maintained. Unlike, sqlite, (a self-contained, serverless, zero-configuration SQL database engine), `acid-state` has plans for a replication, sharding, and other features typically only found in external databases.

While these features are interesting in themselves, what really makes it `acid-state` impressive is that it is exceedingly fast and able to handle huge numbers of simultaneous transactions.

[This acid-state example](http://patch-tag.com/r/stepcut/acid-state-benchmarks/snapshot/current/content/pretty/counter/acid-state/Main.hs) forks 100,000 threads which all compete to update the same counter. On my laptop, it can perform over 25,000 updates per second. It is IO bound by the slow, laptop harddrive -- so a real server should be able to perform significantly better.

As a side note, `acid-state` is currently only ~2000 lines including comments, imports, and blank lines.

Off-the-shelf libraries
=======================

We are currently laying the ground work to be able to offer a selection of off the shelf components making it easy to add authentication, user accounts, payment process, blogs, and more to your website. Stay tuned for more details soon!

Conclusion
==========

Happstack is continuing to innovate and evolve. During Happstack 8 development we will be looking at many of the core technologies in Happstack and figuring out how to bring them more in line with our ideals. This will make Happstack even faster, more robust, and easier to use. As part of this process, we will doing a blog series talking about what we are currently working on. This will force us to justify why the design decisions are in harmony with our philosophy. And if our design is not, then we will have to change it until it is. We will also compare our solutions to solutions offered by other frameworks and show why we think are solution fits better with our ideals. In doing that, we hope to expose weaknesses and oversights of our own designs that we can go back and address. We also hope it will encourage reader feedback. And we also hope it will make it easier to contribute to Happstack by providing documentation explaining why things are they way there are.
