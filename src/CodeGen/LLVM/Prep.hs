{-|
Module      : CodeGen.LLVM.Prep
Description : Map a module to a syntax more amenable to LLVM translation.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable

LLVM IR is a simple language containing functions, declarations, and
simple expressions in SSA form. Toaster has exotic high-level objects
such as closures. This file contains a mapping from elaborated expressions
and a C-like IR called PreLLVM.
-}
module CodeGen.LLVM.Prep ( prep ) where

import Expressions (
  CType(..),
  Expr(..),
  ExprName(..),
  QType(..),
  TlExpr(..),
  TypeName(..) )

import qualified LLVM.AST.Type as T
import Util.DebugOr ( DebugOr, mkSuccess, fromDebugOr )
import Builtins ( builtinsCtx )
import Context ( Ctx, lookupSignature, varValue )
import Data.List ( intercalate )
import Data.Maybe ( mapMaybe )



-- | Transforms a Toaster program to a C-like representation for LLVM code
-- generation.
prep :: [TlExpr] -> DebugOr [TlExpr]
prep tls = do
  let tls' = correctMain tls
  cs <- removeNonConcrete tls'
  resolved <- resolve builtinsCtx cs
  mangled <- mangle resolved
  return $ delambdaTopLevel mangled

-- | Removes all non-concrete code.
removeNonConcrete :: [TlExpr] -> DebugOr [TlExpr]
removeNonConcrete = mkSuccess . mapMaybe onlyIfConcrete
  where
    onlyIfConcrete tl = case tl of
      TlDef _ (Unquantified _) _ -> Just tl
      TlFuncDef {} -> Just tl
      _ -> Nothing

-- | Transform constants that have a lambda initializers to functions.
delambdaTopLevel :: [TlExpr] -> [TlExpr]
delambdaTopLevel = map delambda
  where
    delambda tl = case tl of
      TlDef x (Unquantified (CTArrow _ t)) (EAbs bs e) ->
        TlFuncDef x (deExprName bs) t e
      _ -> tl
    deExprName = map (\(ExprName x, t) -> (x,t))

-- | Transform main into a function.
correctMain :: [TlExpr] -> [TlExpr]
correctMain = map replaceIfMain
  where
    replaceIfMain tl = case tl of
      TlDef "main" (Unquantified t) e -> TlFuncDef "main" [] t e
      _ -> tl

-- | Mangles all names by type to ensure uniqueness.
mangle :: [TlExpr] -> DebugOr [TlExpr]
mangle = traverse mangleTopLevel
  where
    mangleTopLevel (TlDef x qt@(Unquantified ct) e) =
      TlDef <$> name <*> typ <*> expr
      where
        name = appendTypeSuffix x ct
        typ = return qt
        expr = mangleExpr e
    mangleTopLevel (TlFuncDef x bs ct e) =
      TlFuncDef <$> name <*> bindings <*> retType <*> expr
      where
        arrowT = CTArrow (map snd bs) ct
        name = if x == "main" then return x else appendTypeSuffix x arrowT
        bindings = traverse mangleBinding bs
        retType = return ct
        expr = mangleExpr e
    mangleTopLevel (TlDef _ (Quantified _ _ _) _) =
      fail "mangler found a quantified top-level definition"

-- | Appends a type suffix to a string.
-- I would really like `$` to delimit names from types, but Haskell shorts are
-- messed up.
appendTypeSuffix :: String -> CType -> DebugOr String
appendTypeSuffix x t = mangleCType t >>= (\y -> return $ x ++ "." ++ y)

-- | Mangles an expression.
mangleExpr :: Expr -> DebugOr Expr
mangleExpr e = case e of
  EVar (ExprName x) qt@(Unquantified t) -> EVar <$> name <*> return qt
    where
      name = ExprName <$> appendTypeSuffix x t
  EAbs bs e' -> EAbs <$> bs' <*> mangleExpr e'
    where
      bs' = traverse mangleExprBinding bs
  EApp e' es -> EApp <$> mangleExpr e' <*> traverse mangleExpr es
  EIf e1 e2 e3 -> EIf <$> mangleExpr e1 <*> mangleExpr e2 <*> mangleExpr e3
  _ -> mkSuccess e

mangleBinding :: (String, CType) -> DebugOr (String, CType)
mangleBinding (s, t) = do
  s' <- appendTypeSuffix s t
  return (s', t)

mangleExprBinding :: (ExprName, CType) -> DebugOr (ExprName, CType)
mangleExprBinding (ExprName s, t) = do
  s' <- appendTypeSuffix s t
  return (ExprName s', t)


-- | Transform a `CType` to a `String`.
mangleCType :: CType -> DebugOr String
mangleCType t = case t of
  CTBool -> mkSuccess "b"
  CTI32 -> mkSuccess "i32"
  CTArrow argTs resT -> do
    argStrs <- traverse mangleCType argTs
    let argsStr = intercalate "." argStrs
    resStr <- mangleCType resT
    return $ "arrow" ++ argsStr ++ "." ++ resStr ++ ".."
  CTVar (TypeName s) -> fail $ "found type variable `" ++ s ++ "` while mangling"

-- | Transform a CType into an LLVM.AST.Type.
toLlvmType :: CType -> T.Type
toLlvmType t = case t of
  CTBool      -> T.IntegerType 32
  CTI32       -> T.IntegerType 32
  CTArrow _ _ -> T.IntegerType 32
  CTVar _     -> T.IntegerType 32

-- | Resolve built-ins with respect to a context.
resolve :: Ctx -> [TlExpr] -> DebugOr [TlExpr]
resolve ctx = traverse (resolveDef ctx)
  where
    resolveDef :: Ctx -> TlExpr -> DebugOr TlExpr
    resolveDef ctx tl = case tl of
      TlDef x ct e -> TlDef x ct <$> resolveExpr ctx e
      TlFuncDef x bs ct e -> TlFuncDef x bs ct <$> resolveExpr ctx e
    resolveExpr :: Ctx -> Expr -> DebugOr Expr
    resolveExpr ctx e = case e of
      ELitBool _ -> mkSuccess e
      ELitInt _ -> mkSuccess e
      EVar x (Unquantified ct) -> resolveVar ctx x ct
      EVar x Quantified {} -> fail "found a quantified variable while resolving"
      EAbs bs e' -> EAbs bs <$> resolveExpr ctx e'
      EApp e' es ->
        EApp <$> resolveExpr ctx e' <*> traverse (resolveExpr ctx) es
      EIf e1 e2 e3 ->
        EIf <$> resolveExpr ctx e1 <*> resolveExpr ctx e2 <*> resolveExpr ctx e3
      EUnBuiltin {} -> mkSuccess e
      EBinBuiltin {} -> mkSuccess e
    resolveVar :: Ctx -> ExprName -> CType -> DebugOr Expr
    resolveVar ctx x ct = case maybeGetInit ctx x ct of
      Just e@(EUnBuiltin {}) -> mkSuccess e
      Just e@(EBinBuiltin {}) -> mkSuccess e
      _ -> mkSuccess $ EVar x (Unquantified ct)
    maybeGetInit :: Ctx -> ExprName -> CType -> Maybe Expr
    maybeGetInit ctx x ct = fromDebugOr lkp varValue (const Nothing)
      where
        lkp = lookupSignature ctx x (Unquantified ct)

