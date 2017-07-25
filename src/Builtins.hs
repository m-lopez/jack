{-# LANGUAGE FlexibleInstances #-}

module BuiltIns (
  lookupBinaryBuiltin,
  lookupUnaryBuiltin,
  builtinsCtx ) where

import Expressions (
  ExprName(..),
  QType(..),
  CType(..),
  Expr(..),
  areStructurallyEqualQType )
import Contexts ( Ctx(Ctx), extendVars )
import Util.DebugOr ( DebugOr( DebugOr ), mkSuccess, fromMaybe )

import Data.List ( find )



-- | A unary intrinsic operation.
data UnaryIntrinsicData = UnaryIntrinsicData ExprName QType (Expr -> DebugOr Expr)

-- | Retrieve the operation from the intrinsic.
getUnOpCode :: UnaryIntrinsicData -> Expr -> DebugOr Expr
getUnOpCode (UnaryIntrinsicData _ _ op) = op


-- FIXME: Can add another parameter to make sure the output type is the expected
--        type.
class AsUnaryIntrinsicData a where
  asUnaryIntrinsic :: String -> a -> UnaryIntrinsicData

-- Unary integer operations.
instance AsUnaryIntrinsicData (Integer -> Integer) where
  asUnaryIntrinsic x f = UnaryIntrinsicData (ExprName x) t un_op
    where
      t = Unquantified $ CT_arrow [ CT_int ] CT_int
      un_op x = case x of
        E_lit_int y -> mkSuccess $ E_lit_int $ f y
        _ -> fail "expected an integer value; found something else"

-- Unary Boolean operations
instance AsUnaryIntrinsicData (Bool -> Bool) where
  asUnaryIntrinsic x f = UnaryIntrinsicData (ExprName x) t un_op
    where
      t = Unquantified $ CT_arrow [ CT_bool ] CT_bool
      un_op = \x -> case x of
        E_lit_bool y -> mkSuccess $ E_lit_bool $ f y
        _ -> fail "expected an integer value; found something else"

-- | Unary built-in operations.
unaryBuiltins :: [ UnaryIntrinsicData ]
unaryBuiltins = [
  asUnaryIntrinsic "-"   ((\x -> -x) :: Integer -> Integer),
  asUnaryIntrinsic "not" not ]

lookupUnaryBuiltin :: ExprName -> QType -> DebugOr (Expr -> DebugOr Expr)
lookupUnaryBuiltin x t = fromMaybe maybe_op err_msg
  where
    has_sig x t (UnaryIntrinsicData y u _) = x == y && areStructurallyEqualQType t u
    err_msg = "cannot find unary built-in `" ++ show x ++ "`"
    maybe_op = getUnOpCode <$> find (has_sig x t) unaryBuiltins

-- | A Binary intrinsic operation.
data BinaryIntrinsicData =
  BinaryIntrinsicData ExprName QType ((Expr, Expr) -> DebugOr Expr)

-- | Retrieve the binary operation.
getBinOpCode :: BinaryIntrinsicData -> (Expr, Expr) -> DebugOr Expr
getBinOpCode (BinaryIntrinsicData _ _ op) = op

-- FIXME: Can add another parameter to make sure the output type is the expected
--        type.
class AsBinaryIntrinsicData a where
  asBinaryIntrinsic :: String -> a -> BinaryIntrinsicData

-- Binary integer operations.
instance AsBinaryIntrinsicData (Integer -> Integer -> Integer) where
  asBinaryIntrinsic x f = BinaryIntrinsicData (ExprName x) t bin_op
    where
      t = Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int
      bin_op (x,y) = case (x,y) of
        (E_lit_int x, E_lit_int y) -> mkSuccess $ E_lit_int $ f x y
        _ -> fail "expected an integer value; found something else"

-- Binary Boolean operations
instance AsBinaryIntrinsicData (Bool -> Bool -> Bool) where
  asBinaryIntrinsic x f = BinaryIntrinsicData (ExprName x) t bin_op
    where
      t = Unquantified $ CT_arrow [ CT_bool, CT_bool ] CT_bool
      bin_op (x,y) = case (x,y) of
        (E_lit_bool x, E_lit_bool y) -> mkSuccess $ E_lit_bool $ f x y
        _ -> fail "expected an integer value; found something else"

-- Binary integer predicate.
instance AsBinaryIntrinsicData (Integer -> Integer -> Bool) where
  asBinaryIntrinsic x f = BinaryIntrinsicData (ExprName x) t bin_op
    where
      t = Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool
      bin_op (x,y) = case (x,y) of
        (E_lit_int x, E_lit_int y) -> mkSuccess $ E_lit_bool $ f x y
        _ -> fail "expected an integer value; found something else"

-- Need to use coercions?
binaryBuiltins :: [ BinaryIntrinsicData ]
binaryBuiltins = [
  asBinaryIntrinsic "+"   ((+)  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "-"   ((-)  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "*"   ((*)  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "/"   (div  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "rem" (rem  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "or"  ((||) :: Bool -> Bool -> Bool),
  asBinaryIntrinsic "and" ((&&) :: Bool -> Bool -> Bool),
  asBinaryIntrinsic "="   ((==) :: Integer -> Integer -> Bool),
  asBinaryIntrinsic "<>"  ((/=) :: Integer -> Integer -> Bool),
  asBinaryIntrinsic "<"   ((<)  :: Integer -> Integer -> Bool),
  asBinaryIntrinsic "<="  ((<=) :: Integer -> Integer -> Bool),
  asBinaryIntrinsic ">"   ((>)  :: Integer -> Integer -> Bool),
  asBinaryIntrinsic ">="  ((>=) :: Integer -> Integer -> Bool) ]

lookupBinaryBuiltin :: ExprName -> QType -> DebugOr ((Expr, Expr) -> DebugOr Expr)
lookupBinaryBuiltin x t = fromMaybe maybe_op err_msg
  where
    has_sig x t (BinaryIntrinsicData y u _) = x == y && areStructurallyEqualQType t u
    err_msg = "cannot find built-in `" ++ show x ++ "`"
    maybe_op = getBinOpCode <$> find (has_sig x t) binaryBuiltins

unaryOps :: [(ExprName, QType)]
unaryOps = [
  (ExprName "-", Unquantified $ CT_arrow [ CT_int ] CT_int),
  (ExprName "not", Unquantified $ CT_arrow [ CT_bool ] CT_bool) ]

-- | A set of binary operations and their types.
builtinsCtx :: Ctx
builtinsCtx = extendVars ops $ Ctx []
  where
    ops = [
      (ExprName "-",   Unquantified $ CT_arrow [ CT_int ] CT_int),
      (ExprName "not", Unquantified $ CT_arrow [ CT_bool ] CT_bool),
      (ExprName "+",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int),
      (ExprName "-",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int),
      (ExprName "*",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int),
      (ExprName "/",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int),
      (ExprName "rem", Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int),
      (ExprName "or",  Unquantified $ CT_arrow [ CT_bool, CT_bool ] CT_int),
      (ExprName "and", Unquantified $ CT_arrow [ CT_bool, CT_bool ] CT_int),
      (ExprName "=",   Unquantified $ CT_arrow [ CT_bool, CT_bool ] CT_bool),
      (ExprName "=",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool),
      (ExprName "<>",  Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool),
      (ExprName "<>",  Unquantified $ CT_arrow [ CT_bool, CT_bool ] CT_bool),
      (ExprName "<",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool),
      (ExprName "<=",  Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool),
      (ExprName ">",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool),
      (ExprName ">=",  Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool) ]