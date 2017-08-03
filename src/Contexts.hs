module Contexts (
    Ctx(Ctx),
    Binding(BVar),
    extendVars,
    extendVar,
    Value(VExpr,VUnary,VBinary),
    varValue, varName, varType,
    lookupSignature,
    addOrReplaceBinding ) where

import Data.List ( find )
import Util.DebugOr ( DebugOr, justOrErr, mkSuccess, isSuccess )
import Expressions ( Expr, ExprName, QType, areStructurallyEqualQType )

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

-- | True iff a binding has the signature `x: t`.
hasSig :: ExprName -> QType -> Binding -> Bool
hasSig x t (BVar y u _) = x == y && areStructurallyEqualQType t u

-- | Looks up a binding by a signature.
lookupSignature :: Ctx -> ExprName -> QType -> DebugOr Binding
lookupSignature (Ctx bindings) x t = justOrErr binding_maybe err_msg
  where
    binding_maybe = find (hasSig x t) bindings
    err_msg = "cannot find binding with signature `" ++ show x ++ ": " ++ show t ++ "`"

addOrReplaceBinding :: Binding -> Ctx -> DebugOr Ctx
addOrReplaceBinding b@(BVar x t Nothing) ctx@(Ctx bindings) =
  if isSuccess $ lookupSignature ctx x t
    then mkSuccess ctx
    else mkSuccess $ Ctx $ b : bindings
addOrReplaceBinding b@(BVar x t (Just _)) ctx@(Ctx bindings) =
  if isSuccess $ lookupSignature ctx x t
    then mkSuccess $ Ctx $ replaceFirst sameSig b bindings
    else mkSuccess $ Ctx $ b : bindings
  where
    sameSig (BVar y u _) = x == y && areStructurallyEqualQType t u

replaceFirst :: (a -> Bool) -> a -> [a] -> [a]
replaceFirst pred a as = case as of
  []        -> []
  a' : rest -> if pred a' then a : rest else a' : replaceFirst pred a rest
