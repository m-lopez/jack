{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Builtins
Description : Built-in components of the langauge and the intial context.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Builtins ( builtinsCtx ) where

import Expressions (
  ExprName(..),
  QType(..),
  CType(..),
  Expr(..) )
import Context ( Ctx(Ctx), Binding(BVar) )
import Util.DebugOr ( DebugOr, mkSuccess )

-- | Builders for unary intrinsics.
class AsUnaryIntrinsicData a where
  asUnaryIntrinsic :: String -> String -> a -> Binding

-- | A builder for integer transformation intrinsics.
instance AsUnaryIntrinsicData (Integer -> Integer) where
  asUnaryIntrinsic sym id op = BVar (ExprName sym) t (Just $ EUnBuiltin sym id f)
    where
      t = Unquantified $ CTArrow [ CTInt ] CTInt
      f :: Expr -> DebugOr Expr
      f arg = case arg of
        ELitInt n -> mkSuccess $ ELitInt $ op n
        _ -> fail "expected an integer value; found something else"

-- | A builder for Boolean transformation intrinsics.
instance AsUnaryIntrinsicData (Bool -> Bool) where
  asUnaryIntrinsic sym id op = BVar (ExprName sym) t (Just $ EUnBuiltin sym id f)
    where
      t = Unquantified $ CTArrow [ CTBool ] CTBool
      f arg = case arg of
        ELitBool b -> mkSuccess $ ELitBool $ op b
        _ -> fail "expected an Boolean value; found something else"

-- | Builders for binary intrinsics.
class AsBinaryIntrinsicData a where
  asBinaryIntrinsic :: String -> String -> a -> Binding

-- | A binary operation binding builder. 
instance AsBinaryIntrinsicData (Integer -> Integer -> Integer) where
  asBinaryIntrinsic sym id op = BVar (ExprName sym) t (Just $ EBinBuiltin sym id f)
    where
      t = Unquantified $ CTArrow [ CTInt, CTInt ] CTInt
      f args = case args of
        (ELitInt n, ELitInt m) -> mkSuccess $ ELitInt $ op n m
        _ -> fail "expected an integer values; found something else"

-- | A binary integer predicate binding builder.
instance AsBinaryIntrinsicData (Integer -> Integer -> Bool) where
  asBinaryIntrinsic sym id op = BVar (ExprName sym) t (Just $ EBinBuiltin sym id f)
    where
      t = Unquantified $ CTArrow [ CTInt, CTInt ] CTBool
      f args = case args of
        (ELitInt n, ELitInt m) -> mkSuccess $ ELitBool $ op n m
        _ -> fail "expected an integer values; found something else"

-- | A binary Boolean predicate binding builder.
instance AsBinaryIntrinsicData (Bool -> Bool -> Bool) where
  asBinaryIntrinsic sym id op = BVar (ExprName sym) t (Just $ EBinBuiltin sym id f)
    where
      t = Unquantified $ CTArrow [ CTInt, CTInt ] CTBool
      f args = case args of
        (ELitBool x, ELitBool y) -> mkSuccess $ ELitBool $ op x y
        _ -> fail "expected an integer values; found something else"

-- | Builtin context.
builtinsCtx :: Ctx
builtinsCtx = Ctx [
  asUnaryIntrinsic  "-"   "-_I32"   ((\x -> -x) :: Integer -> Integer),
  asUnaryIntrinsic  "not" "not"     not,
  asBinaryIntrinsic "+"   "+_I32"   ((+)  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "-"   "-_I32"   ((-)  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "*"   "*_I32"   ((*)  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "/"   "/_I32"   (div  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "rem" "rem_I32" (rem  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "or"  "or"      (||),
  asBinaryIntrinsic "and" "and"     (&&),
  asBinaryIntrinsic "="   "=_I32"   ((==) :: Integer -> Integer -> Bool),
  asBinaryIntrinsic "<>"  "<>_I32"  ((/=) :: Integer -> Integer -> Bool),
  asBinaryIntrinsic "<"   "<_I32"   ((<)  :: Integer -> Integer -> Bool),
  asBinaryIntrinsic "<="  "<=_I32"  ((<=) :: Integer -> Integer -> Bool),
  asBinaryIntrinsic ">"   ">_I32"   ((>)  :: Integer -> Integer -> Bool),
  asBinaryIntrinsic ">="  ">=_I32"  ((>=) :: Integer -> Integer -> Bool) ]

