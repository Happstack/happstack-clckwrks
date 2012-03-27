How to get Happstack
====================

cabal install happstack
-----------------------

The easiest way to get happstack is to just install it from [hackage](http://www.google.co.uk/search?hl=en&as_sitesearch=hackage.haskell.org/package&as_q=happstack) using the cabal command.

<div class="code">
<pre>
$ export PATH=~/.cabal/bin:$PATH
$ cabal update
$ cabal install happstack
</pre>
</div>

Getting the development version of the source code
--------------------------------------------------

You can get and install the latest development source using [darcs](http://darcs.net/).

<div class="code">
<pre>
$ cabal update
$ darcs get http://patch-tag.com/r/mae/happstack
$ cd happstack
$ chmod +x bin/build-install-all.sh
$ ./bin/build-install-all.sh
</pre>
</code>

After you have checked out the code, you can get further updates by running:

<div class="code">
<pre>
$ darcs pull
</pre>
</div>

To submit a patch run:

<div class="code">
<pre>
$ darcs record
$ darcs send
</pre>
</div>

Note that darcs send assumes you have a working sendmail installed. Another option is to write the patches to a file and then manually attach the patches to an email message. You should send the patches to [our mailing list](http://groups.google.com/group/HAppS).

<div class="code">
<pre>
$ darcs record
$ darcs send -o descriptive_name.dpatch
</pre>
</div>

Browse the source code online
-----------------------------

You can browse the source code online by [visiting our project](http://patch-tag.com/r/mae/happstack/snapshot/current/content/pretty) on patch-tag.com.
