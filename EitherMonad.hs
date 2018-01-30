module EitherMonad where

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure b = Second b
  (<*>) _ (First a) = First a
  (<*>) (First a) _ = First a
  (<*>) (Second fab) (Second a) = Second (fab a)

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b
