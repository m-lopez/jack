{-# LANGUAGE FlexibleInstances #-}

module Evaluator ( evalExpr ) where

import Expressions (
  Expr(..),
  ExprName(ExprName),
  substExprs,
  CType(..),
  QType(..),
  areStructurallyEqualQType )
import Contexts (
  Ctx(Ctx),
  Binding(BVar),
  Value(VUnary, VBinary),
  lookupSignature )
import Util.DebugOr ( DebugOr, requireOrElse, mkSuccess )
import Data.List ( find )



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
evalExpr :: Ctx -> Expr -> DebugOr Expr
evalExpr ctx e = case e of
  ELitBool _ ->
    return e
  ELitInt _ ->
    return e
  EVar _ _ ->
    return e
  EAbs _ _ ->
    return e
  EApp e' es -> evalApp ctx e' es
  EIf c e1 e2 -> do
    res <- evalExpr ctx c
    b   <- asBool res
    if b then evalExpr ctx e1 else evalExpr ctx e2

evalApp :: Ctx -> Expr -> [Expr] -> DebugOr Expr
evalApp ctx e es = do
    vs <- evalExprs ctx es
    f  <- evalExpr ctx e
    case f of
      EAbs xs e' -> evalLambdaApp ctx e' xs vs
      EVar x t   -> evalBuiltin ctx x t vs
      _           -> fail "callee is not callable"

evalLambdaApp :: Ctx -> Expr -> [(ExprName, CType)] -> [Expr] -> DebugOr Expr
evalLambdaApp ctx e xs vs = do
  requireOrElse (length vs == length xs) "arity doesn't match number of arguments"
  let xts = map (\(x,t) -> (x, Unquantified t)) xs
  let subst = zip xts vs
  evalExpr ctx $ substExprs subst e

evalBuiltin :: Ctx -> ExprName -> QType -> [Expr] -> DebugOr Expr
evalBuiltin ctx x t vs = case vs of
  [v]     -> evalUnaryBuiltin ctx x t v
  [v1,v2] -> evalBinaryBuiltin ctx x t (v1, v2)
  _       -> fail ("no built-ins with arity " ++ (show $ length vs))

lookupUnaryBuiltin :: Ctx -> ExprName -> QType -> DebugOr (Expr -> DebugOr Expr)
lookupUnaryBuiltin ctx x t = do
  b <- lookupSignature ctx x t
  getUnaryOp b
  where
    getUnaryOp (BVar _ _ (Just (VUnary op))) = mkSuccess op
    getUnaryOp _ = fail ("`" ++ show x ++ "` does not define a built-in unary")

lookupBinaryBuiltin :: Ctx -> ExprName -> QType -> DebugOr ((Expr, Expr) -> DebugOr Expr)
lookupBinaryBuiltin ctx x t = do
  b <- lookupSignature ctx x t
  getBinaryOp b
  where
    getBinaryOp (BVar _ _ (Just (VBinary op))) = mkSuccess op
    getBinaryOp _ = fail ("`" ++ show x ++ "` does not define a built-in unary")

evalUnaryBuiltin :: Ctx -> ExprName -> QType -> Expr -> DebugOr Expr
evalUnaryBuiltin ctx x t v = do
  op <- lookupUnaryBuiltin ctx x t
  op v

evalBinaryBuiltin :: Ctx -> ExprName -> QType -> (Expr, Expr) -> DebugOr Expr
evalBinaryBuiltin ctx x t vs = do
  op <- lookupBinaryBuiltin ctx x t
  op vs

evalExprs :: Ctx -> [Expr] -> DebugOr [Expr]
evalExprs ctx = traverse (evalExpr ctx)

asBool :: Expr -> DebugOr Bool
asBool e = case e of
  ELitBool b -> return b
  _            -> fail "value is not a Boolean"
