{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Sdh.Memo ( Memoizable(..) ) where

import qualified Data.MemoCombinators as Memo

-- | A type that can be memoized.
class Memoizable a where
  -- The memoize function for the given type.
  memoize :: Memo.Memo a

instance Memoizable Bool where
  memoize = Memo.bool

instance Memoizable () where
  memoize = Memo.unit

instance Integral a => Memoizable a where
  memoize = Memo.integral

instance (Memoizable a, Memoizable b) => Memoizable (a, b) where
  memoize = Memo.pair memoize memoize

instance Memoizable a => Memoizable (Maybe a) where
  memoize = Memo.maybe memoize

