{-# LANGUAGE FlexibleInstances #-}
module BuiltIns (
  lookup_binary_builtin,
  lookup_unary_builtin,
  builtins_ctx ) where

import Expressions (
  Expr_name(..),
  QType(..),
  CType(..),
  Expr(..),
  are_structurally_equal_qtype )
import Contexts ( Ctx(Ctx), extend_vars )
import Util.DebugOr ( DebugOr( DebugOr ), mk_success, from_maybe )

import Data.List ( find )



-- | A unary intrinsic operation.
data UnaryIntrinsicData = UnaryIntrinsicData Expr_name QType (Expr -> DebugOr Expr)

-- | Retrieve the operation from the intrinsic.
get_un_op_code :: UnaryIntrinsicData -> Expr -> DebugOr Expr
get_un_op_code (UnaryIntrinsicData _ _ op) = op


-- FIXME: Can add another parameter to make sure the output type is the expected
--        type.
class AsUnaryIntrinsicData a where
  as_unary_intrinsic :: String -> a -> UnaryIntrinsicData

-- Unary integer operations.
instance AsUnaryIntrinsicData (Integer -> Integer) where
  as_unary_intrinsic x f = UnaryIntrinsicData (Expr_name x) t un_op
    where
      t = Unquantified $ CT_arrow [ CT_int ] CT_int
      un_op = \x -> case x of
        E_lit_int y -> mk_success $ E_lit_int $ f y
        _ -> fail "expected an integer value; found something else"

-- Unary Boolean operations
instance AsUnaryIntrinsicData (Bool -> Bool) where
  as_unary_intrinsic x f = UnaryIntrinsicData (Expr_name x) t un_op
    where
      t = Unquantified $ CT_arrow [ CT_bool ] CT_bool
      un_op = \x -> case x of
        E_lit_bool y -> mk_success $ E_lit_bool $ f y
        _ -> fail "expected an integer value; found something else"

-- | Unary built-in operations.
unary_builtins :: [ UnaryIntrinsicData ]
unary_builtins = [
  as_unary_intrinsic "-"   ((\x -> -x) :: Integer -> Integer),
  as_unary_intrinsic "not" not ]

lookup_unary_builtin :: Expr_name -> QType -> DebugOr (Expr -> DebugOr Expr)
lookup_unary_builtin x t = from_maybe maybe_op err_msg
  where
    has_sig x t (UnaryIntrinsicData y u _) = x == y && are_structurally_equal_qtype t u
    err_msg = "cannot find unary built-in `" ++ show x ++ "`"
    maybe_op = fmap get_un_op_code $ find (has_sig x t) unary_builtins

-- | A Binary intrinsic operation.
data BinaryIntrinsicData =
  BinaryIntrinsicData Expr_name QType ((Expr, Expr) -> DebugOr Expr)

-- | Retrieve the binary operation.
get_bin_op_code :: BinaryIntrinsicData -> (Expr, Expr) -> DebugOr Expr
get_bin_op_code (BinaryIntrinsicData _ _ op) = op

-- FIXME: Can add another parameter to make sure the output type is the expected
--        type.
class AsBinaryIntrinsicData a where
  as_binary_intrinsic :: String -> a -> BinaryIntrinsicData

-- Binary integer operations.
instance AsBinaryIntrinsicData (Integer -> Integer -> Integer) where
  as_binary_intrinsic x f = BinaryIntrinsicData (Expr_name x) t bin_op
    where
      t = Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int
      bin_op = \(x, y) -> case (x,y) of
        (E_lit_int x, E_lit_int y) -> mk_success $ E_lit_int $ f x y
        _ -> fail "expected an integer value; found something else"

-- Binary Boolean operations
instance AsBinaryIntrinsicData (Bool -> Bool -> Bool) where
  as_binary_intrinsic x f = BinaryIntrinsicData (Expr_name x) t bin_op
    where
      t = Unquantified $ CT_arrow [ CT_bool, CT_bool ] CT_bool
      bin_op = \(x, y) -> case (x,y) of
        (E_lit_bool x, E_lit_bool y) -> mk_success $ E_lit_bool $ f x y
        _ -> fail "expected an integer value; found something else"

-- Binary integer predicate.
instance AsBinaryIntrinsicData (Integer -> Integer -> Bool) where
  as_binary_intrinsic x f = BinaryIntrinsicData (Expr_name x) t bin_op
    where
      t = Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool
      bin_op = \(x, y) -> case (x,y) of
        (E_lit_int x, E_lit_int y) -> mk_success $ E_lit_bool $ f x y
        _ -> fail "expected an integer value; found something else"

-- Need to use coercions?
binary_builtins :: [ BinaryIntrinsicData ]
binary_builtins = [
  as_binary_intrinsic "+"   ((+)  :: Integer -> Integer -> Integer),
  as_binary_intrinsic "-"   ((-)  :: Integer -> Integer -> Integer),
  as_binary_intrinsic "*"   ((*)  :: Integer -> Integer -> Integer),
  as_binary_intrinsic "/"   (div  :: Integer -> Integer -> Integer),
  as_binary_intrinsic "rem" (rem  :: Integer -> Integer -> Integer),
  as_binary_intrinsic "or"  ((||) :: Bool -> Bool -> Bool),
  as_binary_intrinsic "and" ((&&) :: Bool -> Bool -> Bool),
  as_binary_intrinsic "="   ((==) :: Integer -> Integer -> Bool),
  as_binary_intrinsic "<>"  ((/=) :: Integer -> Integer -> Bool),
  as_binary_intrinsic "<"   ((<)  :: Integer -> Integer -> Bool),
  as_binary_intrinsic "<="  ((<=) :: Integer -> Integer -> Bool),
  as_binary_intrinsic ">"   ((>)  :: Integer -> Integer -> Bool),
  as_binary_intrinsic ">="  ((>=) :: Integer -> Integer -> Bool) ]

lookup_binary_builtin :: Expr_name -> QType -> DebugOr ((Expr, Expr) -> DebugOr Expr)
lookup_binary_builtin x t = from_maybe maybe_op err_msg
  where
    has_sig x t (BinaryIntrinsicData y u _) = x == y && are_structurally_equal_qtype t u
    err_msg = "cannot find built-in `" ++ show x ++ "`"
    maybe_op = fmap get_bin_op_code $ find (has_sig x t) binary_builtins

unary_ops :: [(Expr_name, QType)]
unary_ops = [
  (Expr_name "-", Unquantified $ CT_arrow [ CT_int ] CT_int),
  (Expr_name "not", Unquantified $ CT_arrow [ CT_bool ] CT_bool) ]

-- | A set of binary operations and their types.
builtins_ctx :: Ctx
builtins_ctx = extend_vars ops $ Ctx []
  where
    ops = [
      (Expr_name "-",   Unquantified $ CT_arrow [ CT_int ] CT_int),
      (Expr_name "not", Unquantified $ CT_arrow [ CT_bool ] CT_bool),
      (Expr_name "+",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int),
      (Expr_name "-",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int),
      (Expr_name "*",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int),
      (Expr_name "/",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int),
      (Expr_name "rem", Unquantified $ CT_arrow [ CT_int, CT_int ] CT_int),
      (Expr_name "or",  Unquantified $ CT_arrow [ CT_bool, CT_bool ] CT_int),
      (Expr_name "and", Unquantified $ CT_arrow [ CT_bool, CT_bool ] CT_int),
      (Expr_name "=",   Unquantified $ CT_arrow [ CT_bool, CT_bool ] CT_bool),
      (Expr_name "=",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool),
      (Expr_name "<>",  Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool),
      (Expr_name "<>",  Unquantified $ CT_arrow [ CT_bool, CT_bool ] CT_bool),
      (Expr_name "<",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool),
      (Expr_name "<=",  Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool),
      (Expr_name ">",   Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool),
      (Expr_name ">=",  Unquantified $ CT_arrow [ CT_int, CT_int ] CT_bool) ]