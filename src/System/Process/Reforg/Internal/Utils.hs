-- | Generic utility definitions.
--
module System.Process.Reforg.Internal.Utils where

import Data.Functor ((<&>))


-- | Monadic version of 'Data.Foldable.any'.
--
-- As defined in https://hackage.haskell.org/package/relude
anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM p = foldr ((||^) . p) (pure False)


-- | Monadic version of 'Data.Bool.(||)'.
--
-- As defined in https://hackage.haskell.org/package/relude
(||^) :: Monad m => m Bool -> m Bool -> m Bool
e1 ||^ e2 = ifM e1 (pure True) e2


-- | Monadic version of @if-then-else@.
--
-- As defined in https://hackage.haskell.org/package/relude
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y


-- | Monadic version of 'Data.Foldable.concatMap'.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (pure [])
    where
      f x xs = op x >>= \ys -> xs <&> (++) ys
