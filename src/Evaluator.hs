{-# LANGUAGE FlexibleInstances #-}

module Evaluator ( eval_expr ) where

import Expressions (
  Expr(..),
  Expr_name,
  subst_expr,
  subst_exprs,
  CType(..), QType(..) )
import Util.DebugOr ( DebugOr, require_or_else )
import BuiltIns ( lookup_unary_builtin, lookup_binary_builtin )



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
eval_expr :: Expr -> DebugOr Expr
eval_expr e = case e of
  E_lit_bool _ ->
    return e
  E_lit_int _ ->
    return e
  E_var _ _ ->
    return e
  E_abs _ _ ->
    return e
  E_app e es -> eval_app e es
  E_if c e1 e2 -> do
    res <- eval_expr c
    b   <- as_bool res
    if b then eval_expr e1 else eval_expr e2

eval_app :: Expr -> [Expr] -> DebugOr Expr
eval_app e es = do
    vs <- eval_exprs es
    f  <- eval_expr e
    case f of
      E_abs xs e' -> eval_lambda_app e' xs vs
      E_var x t   -> eval_builtin x t vs
      _           -> fail "callee is not callable"

eval_lambda_app :: Expr -> [(Expr_name, CType)] -> [Expr] -> DebugOr Expr
eval_lambda_app e xs vs = do
  require_or_else (length vs == length xs) "arity doesn't match number of arguments"
  let xts = map (\(x,t) -> (x, Unquantified t)) xs
  let subst = zip xts vs
  eval_expr $ subst_exprs subst e

eval_builtin :: Expr_name -> QType -> [Expr] -> DebugOr Expr
eval_builtin x t vs = case vs of
  v : []       -> eval_unary_builtin x t v
  v1 : v2 : [] -> eval_binary_builtin x t (v1, v2)
  _            -> fail ("no built-ins with arity " ++ (show $ length vs))

eval_unary_builtin :: Expr_name -> QType -> Expr -> DebugOr Expr
eval_unary_builtin x t v = do
    op <- lookup_unary_builtin x t
    op v

eval_binary_builtin :: Expr_name -> QType -> (Expr, Expr) -> DebugOr Expr
eval_binary_builtin x t vs = do
    op <- lookup_binary_builtin x t
    op vs

eval_exprs :: [Expr] -> DebugOr [Expr]
eval_exprs es = traverse eval_expr es

as_lambda :: Expr -> DebugOr ([(Expr_name, CType)], Expr)
as_lambda e = case e of
  E_abs params e' -> return (params, e')
  _               -> fail "callee is not callable"

as_bool :: Expr -> DebugOr Bool
as_bool e = case e of
  E_lit_bool b -> return b
  _            -> fail "value is not a Boolean"
