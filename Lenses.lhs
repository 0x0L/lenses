An introduction to lenses
=========================

This is a literate Haskell introduction to Lenses.
Total rip-off of an excellent presentation by Simon Peyton Jones
["Lenses: Compositional data access and manipulation"](
https://skillsmatter.com/skillscasts/
4251-lenses-compositional-data-access-and-manipulation).

> {-# LANGUAGE RankNTypes #-}
> module Lenses where

What's in a lense ?
-------------------
Let's start with simple nested records

> data Person = P { name :: String
>                 , addr :: Address
>                 , salary :: Int } deriving (Show)

> data Address = A { road :: String
>                  , city :: String
>                  , postcode :: String } deriving (Show)

We already have the getters. `addr` is such a thing.
Let's define some setters using record notation

> setName :: String -> Person -> Person
> setName n p = p { name = n }

> setPostcode :: String -> Person -> Person
> setPostcode pc p
>   = p { addr = (addr p) { postcode = pc } }

This quickly becomes cumbersome. We want to able to compose
things. Let's simply define a type embedding a getter and
a setter:

> data LensR a b = L { viewR :: a -> b
>                    , setR :: b -> a -> a }

Defining composition is straightforward

> composeL :: LensR a b -> LensR b c -> LensR a c
> composeL (L v1 s1) (L v2 s2) =
>   L (v2 . v1)
>     (\x s -> s1 (s2 x (v1 s)) s)

However this is clearly inefficient if we want to modify a field

> over :: LensR s a -> (a -> a) -> s -> s
> over l f s = setR l (f (viewR l s)) s

We could add a method to do this efficiently instead of using this
combination. But yet, we may also add other methods do deal with
`Maybe` or `IO`

> data LensR' a b = L' { viewR' :: a -> b
>                      , setR' :: b -> a -> a
>                      , mod :: (b -> b) -> a -> a
>                      , modMaybe :: (b -> Maybe b) -> a -> Maybe a
>                      , modIO :: (b -> IO b) -> a -> IO a }

The last two lines look quite similar. They are instances of

> type Lens' s a = forall f. Functor f
>                         => (a -> f a) -> s -> f s

for the `Maybe` and `IO` functors.

Equivalence between `LensR` and `Lens'`
---------------------------------------
Let's start by constructing a `LensR` from a `Lens'`. First the setter:
`f s` must be isomorphic to `s` in order to get the signature right.

Let's define the identity functor:

> data Id a = Id a
>
> runId :: Id a -> a
> runId (Id x) = x
>
> instance Functor Id where
>   fmap f = Id . f . runId

and let's try to get a setter out of a `Lens'`

> set :: Lens' s a -> a -> s -> s
> set ln x = runId . ln (\_ -> Id x)

For the getter we need `f s` to be isomorphic to `a`. Let's introduce
the functor `Const`

> data Const a b = Const a
>
> runConst :: Const a b -> a
> runConst (Const x) = x
>
> instance Functor (Const a) where
>   fmap _ (Const x) = Const x

Using this functor, we can mimic the construction of the setter above.

> get :: Lens' s a -> s -> a
> get ln = runConst . ln Const

That's it ! It's as easy to make the `mod` function out of a lense

> modify :: Lens' s a -> (a -> a) -> s -> s
> modify ln f = runId . ln (Id . f)

To get functions like `modMaybe` or `modIO`, no additional
work is required since these functions already have the same form
as our lense.


We now turn to the task of constructing a `Lens'` from
a `LensR`.

> lens :: (s -> a) -> (a -> s -> s) -> Lens' s a
> lens viewer setter f = \s -> fmap (\a -> setter a s) (f $ viewer s)
