Happstack 8 Roadmap
===================

 1. better infrastructure
 2. a new, faster, safer, and more correct HTTP backend
 3. improve the semantics and clarity of key `happstack-server` functionality
 4. replication and sharding for `acid-state`
 5. plugin/component support


Improved Infrastructure
-----------------------

Now that Happstack 7 is out, we are going to take a brief break from
working on Happstack, and spend some time improving our
infrastructure.

The first thing you will notice is that we have a shiny new site. The
changes are not just skin deep. The new site is based around a CMS,
which makes it much easier for us to keep the site up-to-date.

We also working on a new project: codename `scoutess`. This projects aims to automate and simplify a number of tasks so that we will have more time to focus on the important stuff. Planned features include:

 - continous integration testing against multiple version of the Haskell Platform on multiple operating systems
 - nightly testing that `cabal install` from `hackage` still works
 - automatic building and publishing of Haddock documentation (both stable docs for packages on Hackage, and unstable docs from darcs)
 - automatic notification of updated 3rd party dependencies on `hackage` (very similar to `packdeps`)
 - easy to browse, automatically generated progress reports
 - automatic mirroring of darcs repo to github (via `darcs-bridge` or something)

Mirroring the `happstack` repos on `github` is an especially important task. While we still love `darcs` and are not really keen to switch to `git`, it is clear that we are probably losing contributions by not having a presence on `github`. We are hoping that we can fix this by using `darcs-bridge` to create a live mirror of the official darcs repo on `github`.

New HTTP backend
----------------

The Happstack 7 HTTP backend has held up suprisingly well despite its age. Its performance is on par with `snap` and only 2-4 times slower than `warp`. However, *pretty good* is not good enough. What back in 2009 I filed a [bug report](http://code.google.com/p/happstack/issues/detail?id=29), indicating that Happstack should consider migrating to the newly invented left-fold enumerator. Since then, we have seen a lot of development in the alternatives to lazy IO arena. It feels like the time to make the switch has finally arrived.

So, in Happstack 8, we will be moving forward on a new HTTP backend. The most attractive technology at the moment is the [pipes](http://hackage.haskell.org/package/pipes). `pipes` is a very thoughtfully designed library for working with streams of data. It is based around the very familiar abstractions provided by `Monad` and `Category`. The author has been very careful to pay attention to the semantics of the library and to ensure that it does not violiate any of the `Monad` or `Category` laws. Current work is focused on making sure that it properly handles resource finalization and exception handling with out violiting any of the laws.

We have built a proof-of-concept HTTP server on top of `pipes` and have been pleased with the results so far. The code is easy to understand, and performance is an par with `warp`.

A key aspect of the new HTTP backend will be creating a very high level of assurance that it is actually implemented correctly. We hope to take things to the next level by using grammar driven testing to show that our implementation actually conforms to the specifications. And, where we intentionally deviate from the specifications, we will clearly document why we were forced to deviate (ie., which browser screwed things up).

We also intend to pay special attention to key scalability issues such as per-connection memory usage, and ensuring that server performance does not drop significantly when maximum server capacity is exceeded.

Improvements high-level `happstack-server`
------------------------------------------

We also plan to focus on improving the semantics and design of `happstack-server` functionality resulting in abstractions that are both more powerful and easier to use. For example, the basic routing combinators provided by Happstack, such as `dir` and `path`, are woefully limited. Design is already underway for a new system which is fair more expressive and easier to use than anything else out there.

As part of this development process, we will be blogging about the designs we are working on to ensure that designs are both sound and clear.

`acid-state` improvements
-------------------------

There are three areas of development we would like to see for `acid-state` over the next year: replication/sharding, a better `IxSet`, and an option for swapping data to disk.

Lemmih has sketched out a design for adding replication suport to `acid-state`. This design also appears to have a path forward to sharding as well. We are currently hoping to get replication support for `acid-state` accepted as Google Summer of Code project.

`IxSet` provides a `Set`-like data type that supports multiple indexes. It is, in many ways, similar to a traditional SQL table. While `IxSet` offers a good set of functionality -- the implementation leaves a lot to be desired. There are two similar libraries that look promising `HiggsSet` and `kdtree`. While promising, both these projects could benefit from additional man power. There are also improvemnts which could be made to `IxSet` itself. For example, `IxSet` is not as type-safe as it could be. Both `HiggsSet` and `kdtree` offer similar APIs to `IxSet` but with much better type-safe guarantees. There is no reason why these ideas could not be applied to `IxSet`.

Happstack plugins/components
----------------------------

`web-routes` is best known as the original type-safe URL routing library. What is less known is that it was actually created as the first step of a modular component/plugin architecture for Happstack. Happstack 8 will focus on completing the plugin architecture. `web-routes` was designed to solve two problems:

 1. how to ensure that two plugins do not try to define the same route
 2. how to ensure that developers get a compile time error if a plugin changes the routes it exports

The natural fallout from these requirements is type-safe URLs. type-safe URLs, in turn, provide a number of additional benefits:

 1. the ability to separate the meaning of a URL from the way it is rendered
 2. a easy way to support redirects
 3. compile-time checking for many errors such as typos, invalid paths, etc

The next problems we face regarding plugins are initialization, resource finalization, exception handling, and dependencies between plugins.

Conclusion
----------

I am very excited about what we are doing for Happstack 8. I hope you are too! Contributions are always welcome. If you have your own ideas, don't feel like you have to stick to the plan! We look forward to hearing from you on irc or the mailing list!





