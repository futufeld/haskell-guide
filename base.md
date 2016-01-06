---
layout: page
title: Base
---

* auto-gen TOC:
{:toc}

Functions
---------
You can *compose* functions with `.`:

{% highlight haskell %}
f :: a -> b
g :: b -> c
h :: c -> d
i :: x -> d -> e
x :: x

y, y' :: a -> e
y a = i x (h (g (f a)))
y'  = i x . h . g . f
{% endhighlight %}

{% highlight haskell %}
(.) :: (b -> c) -> (a -> b) -> a -> c
infixr 9 .
{% endhighlight %}

You also can *apply* functions with `$`, the magic parens-go-away wand:

{% highlight haskell %}
a :: a
y, y' :: e
y  = i x (h (g (f a)))
y' = i x $ h $ g $ f a
{% endhighlight %}

{% highlight haskell %}
($) :: (a -> b) -> a -> b
infixr 0 $
{% endhighlight %}

`$` is [right-associative and has a low precedence][ops], so the "invisible parens" stretch all the way to
the end of the line.

Prefer to use `.` over `$`:

{% highlight haskell %}
y, y' :: e
y  = (i . h . g . f) a
y' = i . h . g $ f a
{% endhighlight %}

[ops]: http://www.csinaction.com/2015/03/31/custom-infix-operators-in-haskell/

Case analysis
-------------
A number of data types have case analysis functions, which is like pattern
matching but shorter:

{% highlight haskell %}
f :: a -> r
g :: b -> r

y :: Either a b -> r
y e = case e of
    Left  a -> f a
    Right b -> g b

y' :: Either a b -> r
y' = either f g
{% endhighlight %}

{% highlight haskell %}
Data.Either.either :: (a -> c) -> (b -> c) -> Either a b -> c
Data.Maybe.maybe   :: b        -> (a -> b) -> Maybe a    -> b
{% endhighlight %}

Functors for mapping
--------------------
So you've got a list of `Integer`s, and you want to glue them together sideways
to make a bigger number. Why, who knows.

The first step is to turn your list of numbers into a list of `String`s, because
you can glue a list of strings together with `concat`. We can turn an `Integer`
into a `String` with `show`, and back with `read`.

{% highlight haskell %}
-- These signatures are specialised
concat :: [[a]] -> [a]
show   :: Integer -> String
read   :: String  -> Integer
{% endhighlight %}

But how do we apply `show` to every `Integer` in the list? With `fmap`.

{% highlight haskell %}
-- These signatures are specialised
fmap              :: (Integer -> String) -> [Integer] -> [String]
fmap show         :: [Integer] -> [String]
numbers           :: [Integer]
fmap show numbers :: [String]
{% endhighlight %}

{% highlight haskell %}
fmap :: (Functor f) => (a -> b) -> f a -> f b
{% endhighlight %}

You can also write `fmap` as `<$>`, which can look nicer:

{% highlight haskell %}
show <$> numbers :: [String]
{% endhighlight %}

{% highlight haskell %}
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
infixl 4 <$>
(<$>) = fmap
{% endhighlight %}

`<$>` looks like `$`, which is how you remember it: `$` is function application,
and `<$>` is function application in a context. Your context has to be a
[functor][] though; hence the `(Functor f) => …` bit. And you still pronouce it
"eff map".

So let's do the silly thing:

{% highlight haskell %}
f :: [Integer] -> Integer
f = read . concat . fmap show
{% endhighlight %}

Oh, and there's also [bifunctors][] which have *two* things you can map over.

[functor]: https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Functor.html
[bifunctors]: https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Bifunctor.html

Actions and contexts
--------------------
There are various terms thrown around when talking about functors, monads etc.
in Haskell. This document will use *action* and *context* pretty much
interchangeably.

If you've got a value `IO a`, you can refer to it as an *`IO` action that
returns a value of type `a`*. On the other hand, if you've got a `Maybe a`,
it makes more sense to talk about an *`a` in the `Maybe` context*; the context
of only maybe (tee hee) having a value. Similarly, you could say an `[a]` is a
value `a` in the list context; the context of having 0–n values.

Traversable for action mapping
------------------------------
What if you want to run some action for every `a` in your
list/Maybe/IO/whatever? Well you better hope `Whatever` is
[`Traversable`][traversable], because that's what `traverse` requires:

{% highlight haskell %}
paths    :: [FilePath]
readFile :: FilePath -> IO String

contentses :: IO [String]
contentses = traverse readFile paths
{% endhighlight %}

{% highlight haskell %}
traverse :: (Traversable t, Applicative f)
         => (a -> f b)
         -> t a
         -> f (t b)
{% endhighlight %}

We can't use `fmap` because our actions would end up in the list; a list of
actions that return `String`. That's kinda neat, but not not what we want.
You can turn a list of actions into an action returning a list with `sequenceA`.

{% highlight haskell %}
nope :: [IO String]
nope = readFile <$> paths

yep :: IO [String]
yep = sequenceA nope
{% endhighlight %}

{% highlight haskell %}
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
{% endhighlight %}

If you don't care about the result of your action, use `traverse_`:

{% highlight haskell %}
putStrLn :: String -> IO ()
messages :: [String]

traverse_ putStrLn messages :: IO ()

warning :: Maybe String
traverse_ putStrLn warning :: IO ()
{% endhighlight %}

{% highlight haskell %}
traverse_ :: (Foldable t, Applicative f)
          => (a -> f b)
          -> t a
          -> f ()
{% endhighlight %}

If we used `traverse` above, we'd end up with a list of `()`. Who wants that?

Oh hey, what's `Foldable`?

[traversable]: https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Traversable.html

Foldable for folding
--------------------
[`Foldable`][foldable] is actually a superclass of `Traversable`, but it focuses
on something different: folding over things. Folding is also sometimes called
"reducing" by people who like Hadoop too much. Basically, you take a bunch of
values and fold them into some kind of result.

`foldr` does a lazy *right fold* over some data type. It's normally the fold you
want to use.

{% highlight haskell %}
-- These signatures are specialised
mySum :: [Integer] -> Integer
mySum = foldr (+) 0

setFromList :: [a] -> Set a
setFromList = foldr (\e acc -> Set.insert e acc) Set.empty
{% endhighlight %}

{% highlight haskell %}
foldr :: (Foldable t)
      => (a -> b -> b)  -- Accumulation function
      -> b              -- Initial value
      -> t a            -- Some list-like thing
      -> b
{% endhighlight %}

The thing about our `mySum` is that [it'll crash with long lists][fold-wiki],
because it is lazy in just the wrong spot. `foldl'` fixes that, by being
stricter. Unfortunately, it's also stricter.

Never use `foldl`. It's [terrible][fold-rules].

[foldable]: https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Foldable.html
[fold-wiki]: https://wiki.haskell.org/Foldr_Foldl_Foldl'
[fold-rules]: https://wiki.haskell.org/Foldr_Foldl_Foldl'#Rules_of_Thumb_for_Folds

Applicatives for applying
-------------------------
So we've had `a`s in contexts and containers in contexts. What about functions
in contexts? Enter [`Applicative`][applicative] and `<*>` (pronounced "ap").

`<$>` and `<*>` team up to let you apply a function to a bunch of items in
the same type of context:

{% highlight haskell %}
(++) :: [a] -> [a] -> [a]

paper :: IO String
paper = (++) <$> readFile "abstract.txt"
             <*> readFile "conclusion.txt"
{% endhighlight %}

{% highlight haskell %}
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
infixl 4 <*>
{% endhighlight %}

Remember when we said `<$>` is like `$`? Well `<*>` is actually even more like
`$`, because we stick the function in `f` too. More symmetrical, y'see.

An instance of `Applicative` is called an *applicative functor*, which is why
you see `f` around the place instead of `a` or something.

`Applicative` also let's you put things *in* contexts, with `pure`:

{% highlight haskell %}
-- These signatures are specialised
pure 0 :: [Integer]
pure 0 :: Maybe Integer
pure 0 :: IO Integer
pure 0 :: Either a Integer
{% endhighlight %}

{% highlight haskell %}
pure :: (Applicative f) => a -> f a
{% endhighlight %}

**TODO**: `*>`

[applicative]: https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Applicative.html

Monads for composing
--------------------
Everyone talks about `>>=` (pronounced "bind") when [monads][] are bought up. So
we won't. Let's talk about reverse fish instead.

We've had a couple of `$`-in-contexts, but no `.`-in-context yet. Monads let you
have that with `<=<`, our reverse fish.

{% highlight haskell %}
printFile :: FilePath -> IO ()
printFile = putStrLn <=< readFile
{% endhighlight %}

{% highlight haskell %}
Control.Monad.(<=<) :: (Monad m)
                    => (b -> m c)
                    -> (a -> m b)
                    -> a
                    -> m c
infixr 1 <=<
{% endhighlight %}

Okay, okay, there's also `=<<` ("reverse bind"?). It's nicer than `>>=` because
the data flows in the same direction as normal function application.

{% highlight haskell %}
printFile' :: FilePath -> IO ()
printFile' path = putStrLn =<< readFile path
             -- ≈ putStrLn (readFile path)
{% endhighlight %}

{% highlight haskell %}
Control.Monad.(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
infixr 1 =<<
{% endhighlight %}

Here's a trick: if you want to pattern match on the result of an action, the
`LambdaCase` language extension saves you a lambda by turning `\a -> case a of`
into `\case`:

{% highlight haskell %}
{-# LANGUAGE LambdaCase #-}
printFileStatus :: IO ()
printFileStatus f = doesFileExist f >>= \case
    True  -> pure ()
    False -> putStrLn $ "error: missing file " ++ f
{% endhighlight %}

Be careful of predence when mixing our function and context operators. Here's
a list, highest predence (first applied) to lowest.

* `.` (infixr 9)
* `<$>` and `<*>` (infixl 4)
* `<=<` and `=<<` (infixr 1)
* `$` (infixr 0)

Use parens if things get confusing. Even better, use `where` and name those
subexpressions.

[monads]: https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Monad.html

Monoids for combining
---------------------
We've used `++` a couple of times for appending two lists. But what if we decide
to use `Text` instead of `String`? Do we have to learn a new OK you know where
we're going with this.

A [monoid][] is a thing that you can combine with `<>`. Also, it has an `mempty`
that does nothing when you combine it with something else.

Both `Text` and `String` (all lists, actually) are monoids, but so are `Set`s
and `Ordering`s and bunch of other things.

{% highlight haskell %}
import Data.Text (Text, pack)

"hello" <> " , " <> "world"            :: String
pack "hello, " <> pack "world"         :: Text
Set.empty <> mempty <> Set.singleton 0 :: Set Integer
-- ^   and   ^ are the same for sets
EQ <> LT <> QT                         :: Ordering
-- LT, since <> for Ordering picks the first non-EQ
{% endhighlight %}

{% highlight haskell %}
Data.Monoid.(<>) :: (Monoid m) => m -> m -> m
infixr 6 <>
{% endhighlight %}

The numbers in base don't have `Monoid` instances because there are two ways to
implement it: `<> = +` and `mempty = 0` or `<> = *` and `mempty = 1`.

[monoid]: https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Monoid.html
