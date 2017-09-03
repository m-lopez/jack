{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Evaluator
Description : Types and operations for managing the elaboration context.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Evaluator ( evalExpr ) where

import Expressions (
  Expr(..),
  ExprName,
  substExprs,
  CType(..),
  QType(..) )
import Context (
  Ctx,
  Binding(BVar),
  lookupSignature )
import Util.DebugOr ( DebugOr, requireOrElse, mkSuccess )



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
  EVar x t -> evalVar ctx x t
  EAbs _ _ ->
    return e
  EApp e' es -> evalApp ctx e' es
  EIf c e1 e2 -> do
    res <- evalExpr ctx c
    b   <- asBool res
    if b then evalExpr ctx e1 else evalExpr ctx e2
  e1 -> mkSuccess e1

evalVar :: Ctx -> ExprName -> QType -> DebugOr Expr
evalVar ctx x t = do
  BVar _ _ v_maybe <- lookupSignature ctx x t
  case v_maybe of
    Just e  -> mkSuccess e
    Nothing -> fail $ printed ++ " has no definition"
  where
    printed = "`" ++ show x ++ ": " ++ show t ++ "`"

evalApp :: Ctx -> Expr -> [Expr] -> DebugOr Expr
evalApp ctx e es = do
    vs <- evalExprs ctx es
    -- FIXME: Add value check here.
    f  <- evalExpr ctx e
    case f of
      EAbs xs e'         -> evalLambdaApp ctx e' xs vs
      EUnBuiltin _ _ f'  -> evalUnaryBuiltin f' vs
      EBinBuiltin _ _ f' -> evalBinaryBuiltin f' vs
      _                  -> fail $ "callee " ++ show f ++ " is not callable"

evalLambdaApp :: Ctx -> Expr -> [(ExprName, CType)] -> [Expr] -> DebugOr Expr
evalLambdaApp ctx e xs vs = do
  requireOrElse (length vs == length xs) "arity doesn't match number of arguments"
  let xts = map (\(x,t) -> (x, Unquantified t)) xs
  let subst = zip xts vs
  evalExpr ctx $ substExprs subst e

evalUnaryBuiltin :: (Expr -> DebugOr Expr) -> [Expr] -> DebugOr Expr
evalUnaryBuiltin f vs = case vs of
  [v] -> f v
  _   -> fail $ "unary builtin expects 1 argument; got " ++ (show $ length vs)

evalBinaryBuiltin :: ((Expr, Expr) -> DebugOr Expr) -> [Expr] -> DebugOr Expr
evalBinaryBuiltin f vs = case vs of
  [v1, v2] -> f (v1, v2)
  _        -> fail $ "unary builtin expects 2 argument; got " ++ (show $ length vs)

evalExprs :: Ctx -> [Expr] -> DebugOr [Expr]
evalExprs ctx = traverse (evalExpr ctx)

asBool :: Expr -> DebugOr Bool
asBool e = case e of
  ELitBool b -> return b
  _            -> fail "value is not a Boolean"
