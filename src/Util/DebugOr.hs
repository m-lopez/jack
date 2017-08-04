{-|
Module      : Util.DebugOr
Description : A library for a gathering and reporting compilation errors.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Util.DebugOr (
  DebugOr(..),
  onlySuccessful,
  requireOrElse,
  mkSuccess,
  fromMaybe,
  fail,
  showUnderlying,
  isSuccess,
  justOrErr,
  fromDebug ) where

import Data.Either ( rights )



-- | A monad for debug errors. It wraps `Either` and is right-biased.
newtype DebugOr a = DebugOr { debugRep :: Either String a}

-- | Construct a successful value.
mkSuccess :: a -> DebugOr a
mkSuccess a = DebugOr $ Right a

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
onlySuccessful :: [DebugOr a] -> DebugOr [a]
onlySuccessful dbgs = DebugOr $ Right $ rights $ fmap debugRep dbgs

-- | Construct a nullary `DebugOr` that might be an error.
requireOrElse :: Bool -> String -> DebugOr ()
requireOrElse b msg = case b of
  True  -> return ()
  False -> fail msg

-- | Transforms a `Maybe` into a `DebugOr` and explains the potential `Nothing`.
fromMaybe :: Maybe a -> String -> DebugOr a
fromMaybe a s = case a of
  Just x  -> mkSuccess x
  Nothing -> fail s

-- | Prints either the successful value or the error. Omits `Either` value
--   constructors.
showUnderlying :: Show a => DebugOr a -> String
showUnderlying d = case debugRep d of
  Left x  -> x
  Right x -> show x

-- | True iff the debug value is a success.
isSuccess :: DebugOr a -> Bool
isSuccess (DebugOr x) = case x of
  Left _  -> False
  Right _ -> True

justOrErr :: Maybe a -> String -> DebugOr a
justOrErr a msg = case a of
  Just a' -> mkSuccess a'
  Nothing -> fail msg

-- | Map over each of the cases inot a common type.
fromDebug :: DebugOr a -> (a -> b) -> (String -> b) -> b
fromDebug (DebugOr x) f g = case x of
  Left s  -> g s
  Right y -> f y
