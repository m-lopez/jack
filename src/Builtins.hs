{-# LANGUAGE FlexibleInstances #-}

module BuiltIns ( builtinsCtx ) where

import Expressions (
  ExprName(..),
  QType(..),
  CType(..),
  Expr(..) )
import Contexts ( Ctx(Ctx), Binding(BVar), Value (VUnary, VBinary) )
import Util.DebugOr ( DebugOr, mkSuccess )


-- | Builders for unary intrinsics.
class AsUnaryIntrinsicData a where
  asUnaryIntrinsic :: String -> a -> Binding

-- | A builder for integer transformation intrinsics.
instance AsUnaryIntrinsicData (Integer -> Integer) where
  asUnaryIntrinsic sym op = BVar (ExprName sym) t (Just $ VUnary f)
    where
      t = Unquantified $ CTArrow [ CTInt ] CTInt
      f :: Expr -> DebugOr Expr
      f arg = case arg of
        ELitInt n -> mkSuccess $ ELitInt $ op n
        _ -> fail "expected an integer value; found something else"

-- | A builder for Boolean transformation intrinsics.
instance AsUnaryIntrinsicData (Bool -> Bool) where
  asUnaryIntrinsic sym op = BVar (ExprName sym) t (Just $ VUnary f)
    where
      t = Unquantified $ CTArrow [ CTBool ] CTBool
      f arg = case arg of
        ELitBool b -> mkSuccess $ ELitBool $ op b
        _ -> fail "expected an Boolean value; found something else"

-- | Builders for binary intrinsics.
class AsBinaryIntrinsicData a where
  asBinaryIntrinsic :: String -> a -> Binding

-- | A binary operation binding builder. 
instance AsBinaryIntrinsicData (Integer -> Integer -> Integer) where
  asBinaryIntrinsic sym op = BVar (ExprName sym) t (Just $ VBinary f)
    where
      t = Unquantified $ CTArrow [ CTInt, CTInt ] CTInt
      f args = case args of
        (ELitInt n, ELitInt m) -> mkSuccess $ ELitInt $ op n m
        _ -> fail "expected an integer values; found something else"

-- | A binary integer predicate binding builder.
instance AsBinaryIntrinsicData (Integer -> Integer -> Bool) where
  asBinaryIntrinsic sym op = BVar (ExprName sym) t (Just $ VBinary f)
    where
      t = Unquantified $ CTArrow [ CTInt, CTInt ] CTBool
      f args = case args of
        (ELitInt n, ELitInt m) -> mkSuccess $ ELitBool $ op n m
        _ -> fail "expected an integer values; found something else"

-- | A binary Boolean predicate binding builder.
instance AsBinaryIntrinsicData (Bool -> Bool -> Bool) where
  asBinaryIntrinsic sym op = BVar (ExprName sym) t (Just $ VBinary f)
    where
      t = Unquantified $ CTArrow [ CTInt, CTInt ] CTBool
      f args = case args of
        (ELitBool x, ELitBool y) -> mkSuccess $ ELitBool $ op x y
        _ -> fail "expected an integer values; found something else"

-- | Builtin context.
builtinsCtx :: Ctx
builtinsCtx = Ctx [
  asUnaryIntrinsic  "-"   ((\x -> -x) :: Integer -> Integer),
  asUnaryIntrinsic  "not" not, 
  asBinaryIntrinsic "+"   ((+)  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "-"   ((-)  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "*"   ((*)  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "/"   (div  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "rem" (rem  :: Integer -> Integer -> Integer),
  asBinaryIntrinsic "or"  (||),
  asBinaryIntrinsic "and" (&&),
  asBinaryIntrinsic "="   ((==) :: Integer -> Integer -> Bool),
  asBinaryIntrinsic "<>"  ((/=) :: Integer -> Integer -> Bool),
  asBinaryIntrinsic "<"   ((<)  :: Integer -> Integer -> Bool),
  asBinaryIntrinsic "<="  ((<=) :: Integer -> Integer -> Bool),
  asBinaryIntrinsic ">"   ((>)  :: Integer -> Integer -> Bool),
  asBinaryIntrinsic ">="  ((>=) :: Integer -> Integer -> Bool) ]



-- -- | A Binary intrinsic operation.
-- data BinaryIntrinsicData =
--   BinaryIntrinsicData ExprName QType ((Expr, Expr) -> DebugOr Expr)

-- -- | Retrieve the binary operation.
-- getBinOpCode :: BinaryIntrinsicData -> (Expr, Expr) -> DebugOr Expr
-- getBinOpCode (BinaryIntrinsicData _ _ op) = op

-- -- FIXME: Can add another parameter to make sure the output type is the expected
-- --        type.
-- class AsBinaryIntrinsicData a where
--   asBinaryIntrinsic :: String -> a -> BinaryIntrinsicData

-- -- Binary integer operations.
-- instance AsBinaryIntrinsicData (Integer -> Integer -> Integer) where
--   asBinaryIntrinsic x f = BinaryIntrinsicData (ExprName x) t bin_op
--     where
--       t = Unquantified $ CTArrow [ CTInt, CTInt ] CTInt
--       bin_op (x'',y) = case (x'',y) of
--         (ELitInt x', ELitInt y') -> mkSuccess $ ELitInt $ f x' y'
--         _ -> fail "expected an integer value; found something else"

-- -- Binary Boolean operations
-- instance AsBinaryIntrinsicData (Bool -> Bool -> Bool) where
--   asBinaryIntrinsic x f = BinaryIntrinsicData (ExprName x) t bin_op
--     where
--       t = Unquantified $ CTArrow [ CTBool, CTBool ] CTBool
--       bin_op (x'',y) = case (x'',y) of
--         (ELitBool x', ELitBool y') -> mkSuccess $ ELitBool $ f x' y'
--         _ -> fail "expected an integer value; found something else"

-- -- Binary integer predicate.
-- instance AsBinaryIntrinsicData (Integer -> Integer -> Bool) where
--   asBinaryIntrinsic x f = BinaryIntrinsicData (ExprName x) t bin_op
--     where
--       t = Unquantified $ CTArrow [ CTInt, CTInt ] CTBool
--       bin_op (x'',y) = case (x'',y) of
--         (ELitInt x', ELitInt y') -> mkSuccess $ ELitBool $ f x' y'
--         _ -> fail "expected an integer value; found something else"

-- -- Need to use coercions?
-- binaryBuiltins :: [ BinaryIntrinsicData ]
-- binaryBuiltins = [
--   asBinaryIntrinsic "+"   ((+)  :: Integer -> Integer -> Integer),
--   asBinaryIntrinsic "-"   ((-)  :: Integer -> Integer -> Integer),
--   asBinaryIntrinsic "*"   ((*)  :: Integer -> Integer -> Integer),
--   asBinaryIntrinsic "/"   (div  :: Integer -> Integer -> Integer),
--   asBinaryIntrinsic "rem" (rem  :: Integer -> Integer -> Integer),
--   asBinaryIntrinsic "or"  ((||) :: Bool -> Bool -> Bool),
--   asBinaryIntrinsic "and" ((&&) :: Bool -> Bool -> Bool),
--   asBinaryIntrinsic "="   ((==) :: Integer -> Integer -> Bool),
--   asBinaryIntrinsic "<>"  ((/=) :: Integer -> Integer -> Bool),
--   asBinaryIntrinsic "<"   ((<)  :: Integer -> Integer -> Bool),
--   asBinaryIntrinsic "<="  ((<=) :: Integer -> Integer -> Bool),
--   asBinaryIntrinsic ">"   ((>)  :: Integer -> Integer -> Bool),
--   asBinaryIntrinsic ">="  ((>=) :: Integer -> Integer -> Bool) ]

-- -- | Looks up the code for a built-in.
-- lookupBinaryBuiltin :: ExprName -> QType -> DebugOr ((Expr, Expr) -> DebugOr Expr)
-- lookupBinaryBuiltin x t = fromMaybe maybe_op err_msg
--   where
--     has_sig x' t' (BinaryIntrinsicData y u _) = x' == y && areStructurallyEqualQType t' u
--     err_msg = "cannot find built-in `" ++ show x ++ "`"
--     maybe_op = getBinOpCode <$> find (has_sig x t) binaryBuiltins

-- -- | The set of unary operations.
-- unaryOps :: [(ExprName, QType)]
-- unaryOps = [
--   (ExprName "-", Unquantified $ CTArrow [ CTInt ] CTInt),
--   (ExprName "not", Unquantified $ CTArrow [ CTBool ] CTBool) ]

-- -- | The set of binary operations.
-- binaryOps :: [(ExprName, QType, Value)]
-- binaryOps = [
--   (ExprName "-",   Unquantified $ CTArrow [ CTInt ] CTInt),
--   (ExprName "not", Unquantified $ CTArrow [ CTBool ] CTBool),
--   (ExprName "+",   Unquantified $ CTArrow [ CTInt, CTInt ] CTInt),
--   (ExprName "-",   Unquantified $ CTArrow [ CTInt, CTInt ] CTInt),
--   (ExprName "*",   Unquantified $ CTArrow [ CTInt, CTInt ] CTInt),
--   (ExprName "/",   Unquantified $ CTArrow [ CTInt, CTInt ] CTInt),
--   (ExprName "rem", Unquantified $ CTArrow [ CTInt, CTInt ] CTInt),
--   (ExprName "or",  Unquantified $ CTArrow [ CTBool, CTBool ] CTInt),
--   (ExprName "and", Unquantified $ CTArrow [ CTBool, CTBool ] CTInt),
--   (ExprName "=",   Unquantified $ CTArrow [ CTBool, CTBool ] CTBool),
--   (ExprName "=",   Unquantified $ CTArrow [ CTInt, CTInt ] CTBool),
--   (ExprName "<>",  Unquantified $ CTArrow [ CTInt, CTInt ] CTBool),
--   (ExprName "<>",  Unquantified $ CTArrow [ CTBool, CTBool ] CTBool),
--   (ExprName "<",   Unquantified $ CTArrow [ CTInt, CTInt ] CTBool),
--   (ExprName "<=",  Unquantified $ CTArrow [ CTInt, CTInt ] CTBool),
--   (ExprName ">",   Unquantified $ CTArrow [ CTInt, CTInt ] CTBool),
--   (ExprName ">=",  Unquantified $ CTArrow [ CTInt, CTInt ] CTBool) ]

-- -- | A set of binary operations and their types.
-- -- builtinsCtx :: Ctx
-- -- builtinsCtx = extendVars unaryOps $ extendVars binaryOps $ Ctx []
