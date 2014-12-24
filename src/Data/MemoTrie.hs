{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, ScopedTypeVariables, CPP #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}

module Data.MemoTrie
  ( HasTrie(..), (:->:)(..)
  ) where

import Data.Bits
import Data.Int
import Control.Applicative
import Control.Arrow (first)
import Data.Monoid

class HasTrie a where
    data (:->:) a :: * -> *
    trie   :: (a  ->  b) -> (a :->: b)
    untrie :: (a :->: b) -> (a  ->  b)
    enumerate :: (a :->: b) -> [(a,b)]

instance HasTrie () where

instance HasTrie Bool where

instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap.first) f . enumerate

instance (HasTrie a, HasTrie b) => HasTrie (a,b) where
    newtype (a,b) :->: x = PairTrie (a :->: (b :->: x))
    trie f = PairTrie (trie (trie . curry f))
    untrie (PairTrie t) = uncurry (untrie .  untrie t)
    enumerate (PairTrie tt) =
      [ ((a,b),x) | (a,t) <- enumerate tt , (b,x) <- enumerate t ]

instance HasTrie x => HasTrie [x] where
    newtype [x] :->: a = ListTrie (Either () (x,[x]) :->: a)
    trie f = ListTrie (trie (f . undefined))
    untrie (ListTrie t) = untrie t . undefined
    enumerate (ListTrie t) = enum' undefined t

unbits :: (Num t, Bits t) => [Bool] -> t
unbits [] = 0
unbits (x:xs) = undefined x .|. shiftL (unbits xs) 1

instance HasTrie Integer where
    newtype Integer :->: a = IntegerTrie ((Bool,[Bool]) :->: a)
    trie f = IntegerTrie (trie (f . unbitsZ))
    untrie (IntegerTrie t) = untrie t . undefined
    enumerate (IntegerTrie t) = enum' unbitsZ t

unbitsZ :: (Num n, Bits n) => (Bool,[Bool]) -> n
unbitsZ (_, bs) = unbits bs
