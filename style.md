---
layout: page
title: Style
---

* auto-gen TOC:
{:toc}

Formatting
----------

### Line Length

Prefer 80 columns, but don't waste your time re-working code just to
fit within it. There, 120 is acceptable too. Don't try to shoehorn
code that shouldn't be broken up (like long strings) onto multiple
lines. Let them be.

### Indentation

Tabs are illegal. Use spaces for indenting. Indent your code blocks
with *4 spaces*.

Prefer to indent to a multiple of 4 spaces (2 for `where`); your editor will
thank you.

### Whitespace

Surround binary operators with a single space on either side, including
arithmetic operators. Don't insert a space after a lambda.

### Vertical Aligment

Vertically align the `=` and `->` syntactical elements in guards, case expressions and function clauses to assist readability.

Modules
-------

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

Always document the module header. Format export lists as per the example.

{% highlight haskell %}
{-|
Module      : $Header$
Copyright   : (c) 2016 Deakin Software and Technology Innovation Lab
License     : BSD3

Maintainer  : John Doe <jdoe@swin.edu.au>
Stability   : unstable
Portability : portable

Lorem ipsum dolor sit amet, consectetur adipiscing elit.
-}
module Data.Set (
    -- * The @Set@ type
      Set
    , empty
    , singleton

    -- * Querying
    , member
    ) where
{% endhighlight %}

Language extensions
-------------------

Define `LANGUAGE` pragmas in each file. Do not use `default-extensions`.

Imports
-------

Always use explicit import lists or `qualified` imports for standard and third
party libraries. This makes the code more robust against changes in these
libraries. Exception: The Prelude.

Put imports one empty line after the module header. Always put imports
on separate lines, and sort them alphabetically:

{% highlight haskell %}
module Foo (main) where

import X (foo)
import Y (bar)
{% endhighlight %}

Always group import lists starting with your own project-local imports
first, because they are more important and fewer in number:

{% highlight haskell %}
module Foo.Bar (Bar, bar, mkBar) where

import Foo.Bob (bob)
import Foo.Zot (Fob (Fob), Nob (Nob, Nab), nob)

import Control.Monad ((<=<))
import Data.List.NonEmpty ((:|), nonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import System.Process (proc)
{% endhighlight %}

Write import lists like this:

{% highlight haskell %}
import X (foo, bar)
{% endhighlight %}

And if they don't fit on one line, like this:

{% highlight haskell %}
import X (
      foo, foo'
    , bar )
{% endhighlight %}

Group imported names thematically. When in doubt, sort alphabetically.

If you have many imports, prefer explicit qualification:

{% highlight haskell %}
import qualified X as Z
{% endhighlight %}

Declarations
-------------

One blank line between top-level definitions. No blank lines between
type signatures and function definitions.

Always order declarations in the order that they're used, top-down:

{% highlight haskell %}
main = foo bar

foo = bob

bar = zot
{% endhighlight %}

Always add type-signatures to top-level declarations once they are
written, and document them. For functions the documentation should give enough
information to apply the function without looking at the function's definition.

{% highlight haskell %}
-- | Main entry point.
main :: IO ()
main = foo bar

-- | Pretty print a name.
foo :: String -> IO ()
foo = bob

-- | Default dummy string.
bar :: String
bar = zot

-- | Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
{% endhighlight %}

For readability reasons, don't capitalize all letters when using an
abbreviation. For example, write `HttpServer` instead of
`HTTPServer`. Exception: Two letter abbreviations, e.g. `IO`.

Functions
---------

Use camel case (e.g. `functionName`) when naming functions.

Indent guards 4 spaces.

{% highlight haskell %}
filter _ []     = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
{% endhighlight %}

Prefer shorter functions to longer functions:

{% highlight haskell %}
main = do
    foo bar (z * zz) …
    zot bob (z * zz) …
{% endhighlight %}

(Imagine longer code.)

This is improved by:

{% highlight haskell %}
main = do
    openConnection
    makeCake
  where
    openConnection = foo bar y …
    makeCake = zot bob y …
    y = z * zz
{% endhighlight %}

This better documents what the function is doing.

When the code is larger, this is further improved by:

{% highlight haskell %}
main = do
    openConnection y
    makeCake y
  where
    y = z * zz

openConnection y = foo bar y …

makeCake y = zot bob y …
{% endhighlight %}

Because decoupling allows for more code-reuse and easier to understand
context.

Data types
----------

Use PascalCase when naming data types. Comment every exported data type.
Always put parentheses around derivings.

Align the constructors in a data type definition. Example:

{% highlight haskell %}
-- | Lorem ipsum amet patate.
data Tree a = Branch a (Tree a) (Tree a)
            | Leaf
            deriving (A)
{% endhighlight %}

For long type names the following formatting is also acceptable:

{% highlight haskell %}
-- | Lorem ipsum amet patate.
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
    deriving (Show)
{% endhighlight %}

Format records as follows:

{% highlight haskell %}
-- | Some record type.
data Person = Person
    { firstName :: String  -- ^ First name
    , lastName  :: String  -- ^ Last name
    , age       :: Int     -- ^ Age
    } deriving (Eq, Show)
{% endhighlight %}

For fields that require longer comments format them like so:

{% highlight haskell %}
data Record = Record
    { -- | This is a very very very long comment that is split over
      -- multiple lines.
      field1 :: Text

      -- | This is a second very very very long comment that is
      -- split over multiple lines.
    , field2 :: Int
    }
{% endhighlight %}

Expressions
-----------

Always indent the parent before the children:

{% highlight haskell %}
parent child1 child2
{% endhighlight %}

Or:

{% highlight haskell %}
parent child1
       child2
{% endhighlight %}

Or, when preferred, due to line length constraints:

{% highlight haskell %}
parent
  child1
  child2
{% endhighlight %}

Never mix and match single-line versus multi-line:

{% highlight haskell %}
parent child1 child2  -- NO
       child3
       child4
{% endhighlight %}

### Collections

Write tuples and lists with a single space after the comma:

{% highlight haskell %}
(a, b, c)

[a, b, c]
{% endhighlight %}

Write record update syntax like so:

{% highlight haskell %}
foo { x = a, y = b, z = c }
{% endhighlight %}

Layout multi-line tuples, lists and records with prefix comma:

{% highlight haskell %}
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
{% endhighlight %}

Optionally, you can skip the first newline. Use your judgement.

{% highlight haskell %}
directions = [ North
             , East
             , South
             , West
             ]
{% endhighlight %}

### Hanging lambdas

You may or may not indent the code following a "hanging" lambda. Use
your judgement. Some examples:

{% highlight haskell %}
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
    putStrLn "Here comes a number!"
    print n

foo :: IO ()
foo =
    alloca 10 $ \a ->
    alloca 20 $ \b ->
    cFunction a b
{% endhighlight %}

### If-then-else clauses

Generally, guards and pattern matches should be preferred over if-then-else
clauses, where possible. Short cases should usually be put on a single line
(when line length allows it).

Be consistent with the 4-spaces indent rule, and the `then` and the `else`
keyword should be aligned. Examples:

{% highlight haskell %}
foo = do
    someCode
    if condition
        then someMoreCode
        else someAlternativeCode
{% endhighlight %}

{% highlight haskell %}
foo = bar $ \qux -> if predicate qux
    then doSomethingSilly
    else someOtherCode
{% endhighlight %}

The same rule applies to nested do blocks:

{% highlight haskell %}
foo = do
    instruction <- decodeInstruction
    skip <- load Memory.skip
    if skip == 0x0000
        then do
            execute instruction
            addCycles $ instructionCycles instruction
        else do
            store Memory.skip 0x0000
            addCycles 1
{% endhighlight %}

### Case expressions

The alternatives in a case expression should be indented like so:

{% highlight haskell %}
foobar = case something of
    Just j  -> foo
    Nothing -> bar
{% endhighlight %}

Align the `->` arrows when it helps readability.

### Where clauses

Always indent the `where` keyword 2 spaces, and put definitions 4 spaces in
after a newline:

{% highlight haskell %}
main = do
    hello
    world
  where
    go = print "Hello!"
{% endhighlight %}

### Let expressions

Never use let-expressions in a right-hand-side:

{% highlight haskell %}
main = let x = y  -- NO
       in x
{% endhighlight %}

Instead prefer a `where`:

{% highlight haskell %}
main = x
  where
    x = y
{% endhighlight %}

Write `let` expressions inside a `do` in a bottom-up style:

{% highlight haskell %}
main = do
    a <- return 1
    let x = 123
        y = x * a
    print y
{% endhighlight %}

When in a `do`, avoid `let … in`.

Pragmas
-------

Put pragmas immediately following the function they apply to.
Example:

{% highlight haskell %}
id :: a -> a
id x = x
{-# INLINE id #-}
{% endhighlight %}

In the case of data type definitions you must put the pragma before
the type it applies to. Example:

{% highlight haskell %}
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
{% endhighlight %}

Comments
--------

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

### End-of-line comments

Separate end-of-line comments from the code using 2 spaces. Align
comments for data type definitions. Some examples:

{% highlight haskell %}
data Parser = Parser
    Int         -- Current position
    ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
{% endhighlight %}

### Links

Use in-line links economically. You are encouraged to add links for
API names. It is not necessary to add links for all API names in a
Haddock comment. We therefore recommend adding a link to an API name
if:

* The user might actually want to click on it for more information (in
  your judgment), and

* Only for the first occurrence of each API name in the comment (don't
  bother repeating a link)

Warnings
--------

Code should be compilable with
`-Wall -Werror -Wcompat -fwarn-incomplete-uni-patterns -fwarn-identities`.
There should be no warnings.

Credits
-------

Most of this guide is just segments from [Chris Done][chrisdone]'s and
[Johan Tibell][tibbe]'s style guides. All errors are our own.

[chrisdone]: https://github.com/chrisdone/haskell-style-guide/blob/master/README.md
[tibbe]: https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
