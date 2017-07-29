module Contexts ( Ctx(Ctx), Binding(BVar), extendVars, extendVar ) where

import Expressions ( ExprName, QType )

data Binding = BVar ExprName QType deriving (Show)

newtype Ctx = Ctx [Binding] deriving (Show)

extendVar :: ExprName -> QType -> Ctx -> Ctx
extendVar x t (Ctx ctx) = Ctx $ BVar x t : ctx

extendVars :: [(ExprName, QType)] -> Ctx -> Ctx
extendVars xts (Ctx ctx) = Ctx (new_bindings ++ ctx)
  where
    new_bindings = map (uncurry BVar) xts