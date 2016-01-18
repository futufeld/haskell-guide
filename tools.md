---
layout: page
title: Tools
---

* auto-gen TOC:
{:toc}

Stack
-----

[Stack][] is great. Use it and the latest [Stackage][] LTS when starting new
projects:

{% highlight bash %}
$ stack new --resolver=lts-3.20 my-project
{% endhighlight %}

Hoogle
------

Hoogle lets you search for functions and types in existing packages. Use the
[Stackage][] search function instead of the Haskell.org version.

You can use Hoogle to search for functions have a particular type, or a similar
type. For example, if you're sick of `head` crashing your program when you pass
it invalid input, you could do a search for `(Read a) => String -> Maybe a` and
find the `readMay` function in the `safe` package.

HLint
-----

[HLint][] gives suggestions on how to improve your source code.

You can automatically run HLint over your code on every `stack test` by adding
a new test module `test/HLint.hs`:

{% highlight haskell %}
module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "src"
    , "test"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure
{% endhighlight %}

Then add the following to your `.cabal`:

{% highlight text %}
test-suite hlint
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test
  main-is:             HLint.hs
  build-depends:       base
                     , hlint
  default-language:    Haskell2010
{% endhighlight %}

Never forget to run HLint again!

[HLint]: https://hackage.haskell.org/package/hlint
[Stack]: http://haskellstack.org
[Stackage]: https://www.stackage.org
