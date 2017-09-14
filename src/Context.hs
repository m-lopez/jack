{-|
Module      : Context
Description : Types and operations for managing the elaboration context.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Context (
    Ctx(Ctx),
    Binding(BVar),
    extendVars,
    extendVar,
    declare,
    varValue, varName, varType,
    lookupSignature,
    addOrReplaceBinding,
    addBinding,
    isDef,
    unionCtx ) where

import Data.List ( find )
import Util.DebugOr ( DebugOr, justOrErr, mkSuccess, isSuccess, fromDebugOr )
import Expressions ( Expr, ExprName, QType, areStructurallyEqualQType )

data Binding = BVar {
  varName  :: ExprName,
  varType  :: QType,
  varValue :: Maybe Expr } -- todo: should there be another type of binding instead?

isDef :: Binding -> Bool
isDef b = case b of
  BVar _ _ (Just _) -> True
  BVar _ _ _ -> False

newtype Ctx = Ctx [Binding]

extendVar :: ExprName -> QType -> Expr -> Ctx -> Ctx
extendVar x t v (Ctx ctx) = Ctx $ BVar x t (Just v) : ctx

extendVars :: [(ExprName, QType, Maybe Expr)] -> Ctx -> Ctx
extendVars xts (Ctx ctx) = Ctx (new_bindings ++ ctx)
  where
    new_bindings = map (\(x1, x2, x3) -> BVar x1 x2 x3) xts

unionCtx :: Ctx -> Ctx -> Ctx
unionCtx (Ctx bs1) (Ctx bs2) = Ctx $ bs1 ++ bs2

-- | Add a binding to the context with no initial value.
declare :: ExprName -> QType -> Ctx -> Ctx
declare x t (Ctx bindings) = Ctx $ BVar x t Nothing : bindings

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

addBinding :: Binding -> Ctx -> DebugOr Ctx
addBinding b@(BVar x t Nothing) ctx@(Ctx bindings) =
  if isSuccess $ lookupSignature ctx x t
    then mkSuccess ctx
    else mkSuccess $ Ctx $ b : bindings
addBinding b@(BVar x t (Just _)) ctx@(Ctx bindings) =
  let res = lookupSignature ctx x t in
    if isSuccessAndDef res
      then fail $ "redefining top level definition of `" ++ show x ++ ": " ++ show t ++ "`"
      else if isSuccess res
        then mkSuccess $ Ctx $ replaceFirst sameSig b bindings
        else mkSuccess $ Ctx $ b : bindings
  where
    sameSig (BVar y u _) = x == y && areStructurallyEqualQType t u
    isSuccessAndDef b = fromDebugOr b isDef (const False)

replaceFirst :: (a -> Bool) -> a -> [a] -> [a]
replaceFirst p a as = case as of
  []        -> []
  a' : rest -> if p a' then a : rest else a' : replaceFirst p a rest
