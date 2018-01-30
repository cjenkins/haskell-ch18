module ChapterExercises2 where

import Control.Monad

j :: Monad m => m (m a) -> m a
j mma = join mma

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = fmap f ma

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = (fmap f ma) <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

--?
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = mappend (replicateM 1 (f a)) (meh as f)

flipType :: Monad m => [m a] -> m [a]
flipType = undefined
