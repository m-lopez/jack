module Contexts (
    Ctx(Ctx),
    Binding(BVar),
    extendVars,
    extendVar,
    Value(VExpr,VUnary,VBinary),
    varValue, varName, varType
  ) where

import Util.DebugOr ( DebugOr ) 
import Expressions ( Expr, ExprName, QType )

data Value =
    VExpr Expr
  | VUnary (Expr -> DebugOr Expr)
  | VBinary ((Expr, Expr) -> DebugOr Expr)

data Binding = BVar {
  varName  :: ExprName,
  varType  :: QType,
  varValue :: Maybe Value } -- todo: should there be another type of binding instead?

newtype Ctx = Ctx [Binding]

extendVar :: ExprName -> QType -> Value -> Ctx -> Ctx
extendVar x t v (Ctx ctx) = Ctx $ BVar x t (Just v) : ctx

extendVars :: [(ExprName, QType, Maybe Value)] -> Ctx -> Ctx
extendVars xts (Ctx ctx) = Ctx (new_bindings ++ ctx)
  where
    new_bindings = map (\(x1, x2, x3) -> BVar x1 x2 x3) xts