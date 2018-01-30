module Main where

import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--Nope
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad Nope where
  (>>=) NopeDotJpg f = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = do
    return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

--PhhhbbtttEither
data MyEither b a =
  Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (MyEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap f (Right' b) = Right' b

instance Applicative (MyEither b) where
  pure a = Left' a
  (<*>) _ (Right' b) = Right' b
  (<*>) (Right' b) _ = Right' b
  (<*>) (Left' fab) (Left' a) = Left' (fab a)

instance Monad (MyEither b) where
  return = pure
  (>>=) (Right' b) _ = Right' b
  (>>=) (Left' a) f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Left' a)), (1, return (Right' b))]

instance (Eq a, Eq b) => EqProp (MyEither b a) where
  (=-=) = eq

--Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity fab) (Identity a) = Identity (fab a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--List
--List Applicative
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) as = fmap f as `append` (fs <*> as)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) as f = join $ fmap f as

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    frequency [ (1, return Nil)
              , (3, return (Cons a1 a2))]

instance Eq a => EqProp (List a) where (=-=) = eq

main :: IO ()
main = do
  let nope :: Nope (Int, String, Int)
      nope = undefined
      myEither :: MyEither (Int, String, Int) (String, Int, String)
      myEither = undefined
      myIdentity :: Identity (Int, String, Int)
      myIdentity = undefined
      myList :: List (Int, String, Int)
      myList = undefined
  quickBatch $ functor nope
  quickBatch $ applicative nope
  quickBatch $ monad nope
  quickBatch $ functor myEither
  quickBatch $ applicative myEither
  quickBatch $ monad myEither
  quickBatch $ functor myIdentity
  quickBatch $ applicative myIdentity
  quickBatch $ monad myIdentity
  quickBatch $ functor myList
  quickBatch $ applicative myList
  quickBatch $ monad myList
