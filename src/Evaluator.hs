{-# LANGUAGE FlexibleInstances #-}

module Evaluator ( evalExpr ) where

import Expressions (
  Expr(..),
  ExprName,
  substExprs,
  CType(..), QType(..) )
import Util.DebugOr ( DebugOr, requireOrElse )
import BuiltIns ( lookupUnaryBuiltin, lookupBinaryBuiltin )



--------------------------------------------------------------------------------
--  Evaluation system
--
-- Based on small-step semantics:
--
--  e_i ~> v => e(e_1, ..., e_i, ..., e_n) ~> e (e_1, ..., v, ..., e_n)
--  e ~> \(x_1, ..., x_n).e' => e (v_1, ..., v_n) ~> e' [v_1 -> x_1, ..., v_n -> x_n]
--  e1 ~> v => if e1 then e2 else e3 ~> if v then e2 else e3
--  if true then e1 else e2 ~> e1
--  if false then e1 else e2 ~> e2

-- We allow Maybe to catch compiler errors that lead to an unsound type system.
evalExpr :: Expr -> DebugOr Expr
evalExpr e = case e of
  ELitBool _ ->
    return e
  ELitInt _ ->
    return e
  EVar _ _ ->
    return e
  EAbs _ _ ->
    return e
  EApp e' es -> evalApp e' es
  EIf c e1 e2 -> do
    res <- evalExpr c
    b   <- asBool res
    if b then evalExpr e1 else evalExpr e2

evalApp :: Expr -> [Expr] -> DebugOr Expr
evalApp e es = do
    vs <- evalExprs es
    f  <- evalExpr e
    case f of
      EAbs xs e' -> evalLambdaApp e' xs vs
      EVar x t   -> evalBuiltin x t vs
      _           -> fail "callee is not callable"

evalLambdaApp :: Expr -> [(ExprName, CType)] -> [Expr] -> DebugOr Expr
evalLambdaApp e xs vs = do
  requireOrElse (length vs == length xs) "arity doesn't match number of arguments"
  let xts = map (\(x,t) -> (x, Unquantified t)) xs
  let subst = zip xts vs
  evalExpr $ substExprs subst e

evalBuiltin :: ExprName -> QType -> [Expr] -> DebugOr Expr
evalBuiltin x t vs = case vs of
  [v]     -> evalUnaryBuiltin x t v
  [v1,v2] -> evalBinaryBuiltin x t (v1, v2)
  _       -> fail ("no built-ins with arity " ++ (show $ length vs))

evalUnaryBuiltin :: ExprName -> QType -> Expr -> DebugOr Expr
evalUnaryBuiltin x t v = do
    op <- lookupUnaryBuiltin x t
    op v

evalBinaryBuiltin :: ExprName -> QType -> (Expr, Expr) -> DebugOr Expr
evalBinaryBuiltin x t vs = do
    op <- lookupBinaryBuiltin x t
    op vs

evalExprs :: [Expr] -> DebugOr [Expr]
evalExprs = traverse evalExpr

asBool :: Expr -> DebugOr Bool
asBool e = case e of
  ELitBool b -> return b
  _            -> fail "value is not a Boolean"
