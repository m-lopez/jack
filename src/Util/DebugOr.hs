-----------------------------------------------------------------------------
-- |
-- Module      : Util.DebugOr
-- Copyright   : (c) Michael Lopez 2017
-- License     : MIT
--
-- Maintainer  : Michael Lopez
-- Stability   : experimental
--
-- A library for a gathering and reporting compilation errors.
--

module Util.DebugOr (
  DebugOr(..),
  only_successful,
  require_or_else,
  mk_success,
  from_maybe,
  fail,
  show_underlying
) where

import Data.Either ( lefts, rights )
import Data.List ( intercalate )



-- | A monad for debug errors. It wraps `Either` and is right-biased.
newtype DebugOr a = DebugOr { debug_rep :: Either String a}

-- | Construct a successful value.
mk_success :: a -> DebugOr a
mk_success a = DebugOr $ Right a

-- | The `DebugOr` type is a `Functor`.
instance Functor DebugOr where
  fmap f dx = let DebugOr x = dx in case x of
    Left err -> DebugOr $ Left err
    Right x' -> DebugOr $ Right $ f x'

-- | The `DebugOr` type is a `Applicative`.
-- While applicative is supported, `DebugOr` supports the monadic-style of
-- handling failure, meaning an error short-circuits. In the future, it should
-- probably accumulate.
instance Applicative DebugOr where
  pure x = DebugOr $ Right x
  f <*> x =
    let
      DebugOr f' = f
      DebugOr x' = x
    in case (f', x') of
      (Right f'', Right x'') -> DebugOr $ Right $ f'' x''
      (Left err, _)          -> DebugOr $ Left err
      (_, Left err)          -> DebugOr $ Left err

-- | The `DebugOr` type is a `Monad`.
instance Monad DebugOr where
  return x = DebugOr $ Right x
  x >>= f = let DebugOr x' = x in case x' of
    Left msg -> fail msg
    Right y  -> f y
  fail msg = DebugOr $ Left msg

-- | Discard debug errors.
only_successful :: [DebugOr a] -> DebugOr [a]
only_successful dbgs = DebugOr $ Right $ rights $ fmap debug_rep dbgs

-- | Construct a nullary `DebugOr` that might be an error.
require_or_else :: Bool -> String -> DebugOr ()
require_or_else b msg = case b of
  True  -> return ()
  False -> fail msg

-- | Transforms a `Maybe` into a `DebugOr` and explains the potential `Nothing`.
from_maybe :: Maybe a -> String -> DebugOr a
from_maybe a s = case a of
  Just x  -> mk_success x
  Nothing -> fail s

-- | Prints either the successful value or the error. Omits `Either` value
--   constructors.
show_underlying :: Show a => DebugOr a -> String
show_underlying d = case debug_rep d of
  Left x  -> x
  Right x -> show x
