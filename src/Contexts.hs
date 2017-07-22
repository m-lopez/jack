module Contexts ( Ctx(Ctx), Binding(B_var), extend_vars ) where

import Expressions ( Expr_name, QType )

data Binding = B_var Expr_name QType deriving (Show)

newtype Ctx = Ctx [Binding] deriving (Show)

extend_var :: Expr_name -> QType -> Ctx -> Ctx
extend_var x t (Ctx ctx) = Ctx $ (B_var x t) : ctx

extend_vars :: [(Expr_name, QType)] -> Ctx -> Ctx
extend_vars xts (Ctx ctx) = Ctx (new_bindings ++ ctx)
  where
    new_bindings = map (uncurry B_var) xts