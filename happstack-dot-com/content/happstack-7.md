Happstack 7 Release Notes
=========================

I am pleased to announce the official release of Happstack 7. Happstack is a fast, modern Haskell web framework focused on bringing the uncomprised beautify and power of Haskell to a web framework.

During the Happstack 7 development cycle, we have been able to release code to hackage as it was written. As a result, Happstack 7 does not contain any new code that is not already on hackage. However this release is still significant in several ways. As part of this release we are officially deprecating several packages. Additionally, the release of Happstack 7 marks the end of a period of incremental improvement, and the beginning of a new period of develop that will see some more radical changes.

Since Happstack 6, we have made many improvements to the code and added significantly more sections to the Happstack Crash Course.

deprecated packages and functions
---------------------------------

One reason for releasing Happstack 7 is that it gives us the excuse to now official deprecate and rename some packages. One long term goal has to free `happstack-state` and the related packages of the `happstack` moniker. It has been true for a long time that you could use `happstack-data`, `happstack-state` and `happstack-ixset` independently from `happstack`. But the naming implied otherwise.

 - `happstack-utils` is now deprecated in favor of nothing. If you were using anything from this library -- you should probably find a replacement that is actively maintained. Especially if you were using the Crypto related functions.
 - `happstack-data` is now deprecated in favor of [safecopy](http://acid-state.seize.it/safecopy). `safecopy` offers binary serialization with automatic version migration.
 - `happstack-state` is now deprecated in favor of `acid-state`.
 - `happstack-ixset` has been renamed to `ixset` and `Happstack.Data.IxSet.*` has been renamed to `Data.IxSet.*`. Additionally it now depends on `safecopy` instead of `happstack-data`.
 - `happstack-init` - the `happstack init` command from the `happstack` package has been moved into a new package `happstack-init`, which is currently marked as deprecated unless a new maintainer steps forward.
 - in `happstack-server` the various numerous method* functions deprecated in favor of single `method` function


`acid-state` and `safecopy`
---------------------------

`acid-state` is a high performance library for adding ACID guarantees to (nearly) any kind of Haskell data structure. In layman's term, it's a library that will make your data persist even in the event of, say, a power outage. Unplug your machine and restart and have your app recover to exactly where it left off. `acid-state` spares you the need to deal with all the marshalling, consistency, and configuration headache that you would have if you used an external DBMS for this purpose.

One of the key goals for Happstack 7 was to make significant improvements to `happstack-state`. Lemmih single-handledly resolved a vast majority of the bugs filed against `happstack-state` and `happstack-data` by rewriting them from the ground up as the new `acid-state` and `safecopy` libraries.

As a result, we are now officially deprecating `happstack-state` and `happstack-data` in favor of `acid-state` and `safecopy`. You are encouraged to migrate at your soonest convienced. However, we will still continue to ensure that `happstack-state` and `happstack-data` build, so there is no urgent reason why you must upgrade.

When you are ready to migrate you can follow this guide for [happstack-state to acid-state migration](http://code.google.com/p/happstack/wiki/HapstackStateToAcidState).

 - acid-state does not require a global `IORef` like happstack-state did
 - easier to migrate components when the name of the type changes
 - components are not silently deleted when unused
 - easy to specify initial test in unit tests
 - correct behavior when an update/query method throws an exception
 - no longer contains secret command-line arguments


happstack-lite
--------------

Happstack has a full-featured API, which can be overwhelming at first. In order to address this problem, we created a new package named `happstack-lite`.

`happstack-lite` provides a simplified way to get started with Happstack with out giving up the ability to transition seamlessly to the full Happstack later. The [getting started with happstack-lite](http://happstack.com/docs/happstack-lite/happstack-lite.html) tutorial includes more information.

happstack-server-tls
--------------------

Happstack now includes a new, optional `happstack-server-tls` package. This package makes it easy to add native `https://` support to your web application. (No reverse proxy/apache/etc. required).

hsx-jmacro
----------

The `hsx-jmacro` package integrates the `hsx` and `jmacro` libraries so that it is easy to include Javascript in your `HSX` templates. This includes support for splicing Haskell values into javascript, javascript code generation, and more. The Happstack Crash Course includes a new section on [using JMacro with HSX](http://happstack.com/docs/crashcourse/Templates.html#jmacro).

Happstack and I18N
------------------

Happstack now includes support for internationalization (I18N). The Happstack Crash Course now includes a section on [HSX and I18N](http://happstack.com/docs/crashcourse/Templates.html#hsp-i18n).

What else is new?
-----------------

<h3><code>happstack-server</code></h3>

 - FromReqURI instances for `Text` and lazy `Text`
 - add `lookText` and `lookTexts` for strict and lazy `Text`
 - added `Happstack` instance for `ReaderT`, `StateT`, `WriterT`, and `RWST`
 - `serveDirectory` directory listings now sorted alphabetically
 - `method GET` now implicitly matches on `HEAD` requests as well
 - moved `waitForTermination` from deprecated `happstack-state` into `happstack-server`
 - added `ToSURI` instance for `Text`
 - added `prettyResponse` function for pretty-printing the `Response` (makes debugging easier)
 - added `ToMessage` instances for `ByteString` and lazy `ByteString`
 - added `bindIPv4` to make it easier to bind to `IPv4` socket
 - added `Alternative` and `Applicative` as constraints to the `Happstack` class
 - added `httpOnly` flag to `Cookie`

<h3>Other</h3>
 - happstack-hsp: added async google analytics code
 - happstack-plugins: split into two packages: plugins-auto (does not depend on Happstack) and happstack-plugins (integration into Happstack)
 - ixset: added `groupAscBy`, `groupDescBy`, `toAscList` and `toDescList`

fixes
-----

Happstack 7 includes many trivial updates to keep up-to-date with the latest available dependencies. Some of more the more interesting bugs fixes since Happstack 6 include:

 - updates to compile against GHC 7.4
 - fixed issue decoding 'multipart/form-data'
 - fixed `happstack-data`/`happstack-state` to work correctly with new `Show` instance for `TypeRep`
 - fixed decoding of Unicode paths
 - do not unescape + in path segments
 - allow for empty cookies
 - fixed bug in SHA1 due to changes in the way `shiftL` handles negative values
 - `happstack-server` now builds with out any compiler warnings
 - fixes to work around potential DoS attack when using `read` to decode `Integral` values
 - fixed `plugins-auto` to only recompile once when there is a cluster set of file modification events
 - prevent plugins from unloading in the presence of compilation errors
 - better `plugins-auto` support for hierarchical modules
 - fixed `inotify` code in `plugins-auto` to notice changes correctly when files are saved using `vim`
 - use `offset` when sending with `sendfile`
 - changed `RqEnv` so that it is possible to look at query string values with out calling `decodeBody` first
 - fix safepath calculation in `serveDirectory`
 - fix how cookie parser decodes quoted pairs
 - in `decodeBody` return `requestEntityTooLarge` instead of calling `fail` when quote is exceeded.
 - fix keep-alive for http 1.0 when there is no content-length set.
 - fixed file handle leak in `filePathSendFile` and `filePathStrict`
