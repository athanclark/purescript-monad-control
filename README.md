# purescript-monad-control

This is a clone of Bas van Dijk and Anders Kaseorg's [monad-control](http://hackage.haskell.org/package/monad-control)
library for purescript.

What is the point of this library? From what I see, monad transformers only give you a one-way valve - you can lift "up",
but "running" a transformer is specific for each one. What this library provides is a method to "going down, then coming up".
It is essentially a continuation for monad morphisms. This is extremely useful when you need to run a high-level action
in a lower one (like as `IO` or `Eff`), before lifting it back up:

```haskell
async' :: (MonadBaseControl IO m) => m a -> m (Async a)
asnyc' x = do
  liftBaseWith $ \runInBase ->
    async $ (runInBase x :: IO a)
```
