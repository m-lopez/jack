{-|
Module      : Elaboration
Description : Types and operations for managing the elaboration context.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Elaboration (
  synthExpr,
  checkExpr,
  checkType,
  checkTopLevelBinding,
  elabModule ) where

import Syntax.Ast (
  Ast(..), Module(..), LocalContext(..), LocalConstantContext(..),
  TopLevel(..), ConstantParameter(..), BuiltinType(..) )
import qualified Syntax.Ast as A
import Util.DebugOr (
  DebugOr,
  onlySuccessful,
  requireOrElse,
  mkSuccess,
  fromDebugOr )
import Expressions (
  Expr(..),
  CType(..),
  QType(..),
  ExprName(..),
  TypeName(..),
  TlExpr(..),
  areStructurallyEqualCType,
  areStructurallyEqualQType )
import Context (
  Ctx(..),
  Binding(BVar),
  extendVars,
  lookupSignature,
  addBinding,
  isDef, unionCtx )
import Data.Maybe ( isJust )
import Data.List ( find )


-- | Elaboorate a module.
elabModule :: Ctx -> A.Module -> DebugOr [TlExpr]
elabModule externCtx (A.Module _ tls) = do
  localCtx <- elabDeclarations externCtx tls
  requireUniqueDecls localCtx
  let ctx = externCtx `unionCtx` localCtx
  elabTops ctx tls

-- | Report a message if the reflaxive, symmetric relation is satisfied by any
-- two distinct elements in the list.
requireUnrelated :: [a] -> (a -> a -> Bool) -> (a -> a -> String) -> DebugOr ()
requireUnrelated xs r mkMsg = case xs of
  [] -> mkSuccess ()
  x:xs' -> do
    findRelation x xs r mkMsg
    requireUnrelated xs' r mkMsg
  where
    findRelation y ys r mkMsg = case find (r y) ys of
      Just y' -> fail $ mkMsg y y'
      Nothing -> mkSuccess ()

-- | Fail if there is more than one declaration for two types.
requireUniqueDecls :: Ctx -> DebugOr ()
requireUniqueDecls (Ctx bs) = requireUnrelated
  bs
  (\(BVar (ExprName x1) qt1 _) (BVar (ExprName x2) qt2 _) -> x1 == x2 && areStructurallyEqualQType qt1 qt2)
  (\(BVar (ExprName x1) qt1 _) _ -> "found two bindngs for `" ++ x1 ++ ": " ++ show qt1 ++ "`")

-- | Populate the top-level context with .
elabDeclarations :: Ctx -> [A.TopLevel] -> DebugOr Ctx
elabDeclarations ctx tls = Ctx <$> traverse (elabDecl ctx) tls
  where
    elabDecl ctx' tl = case tl of
      A.ConstantDef x _ tp _ -> do
        t <- checkType ctx' tp
        return $ BVar (ExprName x) t Nothing
      _ -> fail "unsupported"

-- | Elaborate top-levels.
elabTops :: Ctx -> [A.TopLevel] -> DebugOr [TlExpr]
elabTops ctx = mapM (elabTop ctx)
  where
    elabTop ctx' ast' = case ast' of
      A.ConstantDef x params t e -> elabConstantDef ctx' x params t e
      _ -> fail "unsupported-001"

-- | Elaborate a constant definition.
elabConstantDef :: Ctx -> String -> A.LocalContext -> A.Ast -> A.Ast -> DebugOr TlExpr
elabConstantDef ctx x l t_p e_p = do
  let LocalContext cparams cst params = l
  if isJust cparams || isJust cst || isJust params
    then fail "unsupported-002"
    else do
      t <- checkType ctx t_p
      (e, _) <- checkExpr ctx e_p t
      return $ TlDef x t e

-- | A helper function to convert AST symbols to expression symbols.
toExprName :: String -> ExprName
toExprName = ExprName

-- | The type of an overload set.
newtype OverloadSet = OverloadSet { interps :: [(Expr, QType)] }

-- Print just the immediate node.
summerizeForm :: Ast -> String
summerizeForm ast = case ast of
  AArrowType  _ _  -> "_ -> _"
  AName     s -> "`" ++ (head s) ++ "`"
  AAbs _ _    -> "\\(x:t,...) -> e"
  AApp _ _    -> "e e"
  AIf _ _ _   -> "if b then e else e"
  _           -> "unrecognized AST"

--------------------------------------------------------------------------------
-- Requirement combinators.

requireSingleton :: OverloadSet -> Ast -> DebugOr (Expr, QType)
requireSingleton ovlds ast = case interps ovlds of
  []  -> fail $ "no viable interpretation of " ++ summerizeForm ast
  [v] -> return v
  _   -> fail $ "ambiguous interpretation for " ++ summerizeForm ast

requireUnquantifiedType :: QType -> DebugOr CType
requireUnquantifiedType qt = case qt of
  Quantified _ _ _ -> fail "cannot convert type to unquantified"
  Unquantified t   -> return t

requireArity :: [a] -> [b] -> DebugOr ()
requireArity xs ys =
  if length xs == length ys then return () else fail "different lengths"

requireArrowType :: QType -> DebugOr ([CType], CType)
requireArrowType qt = case qt of
  Unquantified (CTArrow t1 t2) -> return (t1, t2)
  _ -> fail $ "expected an arrow type; got a " ++ show qt

requireTypeEqs :: [CType] -> [CType] -> DebugOr ()
requireTypeEqs ts us = case length ts == length us of
  False -> fail $ "types not equal: unimplemented"
  True  -> case all (uncurry areStructurallyEqualCType) $ zip ts us of
    False -> fail $ "type mismatch: unimplmented"
    True  -> return ()



--------------------------------------------------------------------------------
--  Overload resolution.

-- Attempt to type check `f:t` applied to the arguments `ps` with respect to the
-- context `ctx`.
viable :: Ctx -> Expr -> QType -> [Ast] -> DebugOr (Expr, QType)
viable ctx f t ps = do
  (src_ts, tgt_t) <- requireArrowType t
  _               <- requireArity ps src_ts
  typed_args      <- checkExprs ctx $ zip ps $ fmap Unquantified src_ts
  let args = map fst typed_args in
    return (EApp f args, Unquantified tgt_t)

-- Perform C++-style overload resolution for the call `p ps`.
resolve :: Ctx -> Ast -> [Ast] -> DebugOr OverloadSet
resolve ctx p ps = case p of
  AName n -> do
    cands   <- lookupVar (head n) ctx                          -- Computes a candidate set
    viables <- onlySuccessful $ map (\(f, t) -> viable ctx f t ps) $ interps cands  -- Computes viable functions
    _       <- requireOrElse (not $ null viables) ("no valid interpretations for application" ++ show p)
    return $ OverloadSet viables
  -- FIXME: Should support lambdas.
  ast -> fail $ "attempted overload resolution on an uncallable expression; got " ++ show ast







--------------------------------------------------------------------------------
--  Lookup and selection system.
--
--  This system is responsible for querying the context for bindings.

-- Lookup an expression variable.
lookupVar :: String -> Ctx -> DebugOr OverloadSet
lookupVar n (Ctx bindings) =
  let
    var_binding_has_name v b = case b of BVar v' _ _ -> v == v'
    overloads = filter (var_binding_has_name $ ExprName n) bindings
  in mkSuccess $ OverloadSet $
    map (\(BVar x t _) -> (EVar x t, t)) overloads

-- Select from an overload on type.
selectByType :: QType -> OverloadSet -> DebugOr (Expr, QType)
selectByType t (OverloadSet ovld) =
  case filter (areStructurallyEqualQType t . snd) ovld of
    []  -> fail $ "no viable overload matches the type " ++ show t
    [x] -> return x
    _   -> fail "ambiguous selection"



--------------------------------------------------------------------------------
--  Kind checking.

checkType :: Ctx -> Ast -> DebugOr QType
checkType ctx p = Unquantified <$> checkUnquantType ctx p

checkUnquantType :: Ctx -> Ast -> DebugOr CType
checkUnquantType ctx p = case p of
  ABuiltinType bt -> checkBuiltinType bt
  ARecType bs -> checkRecType ctx bs
  AArrowType ps p' -> do
    src_ts <- checkUnquantTypes ctx ps
    tgt_t  <- checkUnquantType ctx p'
    return $ CTArrow src_ts tgt_t
  AName xt -> let n = head xt in return $ CTVar $ TypeName n
  _ -> fail_here
  where fail_here = fail $ "expected unquantified type; got " ++ summerizeForm p

checkBuiltinType :: BuiltinType -> DebugOr CType
checkBuiltinType bt = case bt of
  U8 -> fail "unsupported"
  U16 -> fail "unsupported"
  U32 -> fail "unsupported"
  U64 -> fail "unsupported"
  I8 -> fail "unsupported"
  I16 -> fail "unsupported"
  I32 -> mkSuccess CTI32
  I64 -> fail "unsupported"
  BoolT -> mkSuccess CTBool
  F32 -> fail "unsupported"
  F64 -> fail "unsupported"

checkRecType :: Ctx -> [A.Binding] -> DebugOr CType
checkRecType _ = fail "nothing"

checkUnquantTypes :: Ctx -> [Ast] -> DebugOr [CType]
checkUnquantTypes ctx = traverse (checkUnquantType ctx)



--------------------------------------------------------------------------------
--  Type checking.
--
--  This is a system for transforming untyped ASTs to typed elaborations.
--
--  This language uses a bidirectional type system [Pierce reference.] Type
--  elaboration is separated into type checking rules `checkExpr` and
--  respective type inference rules `synthExpr`.

-- Synthesize a typed expression from an AST.
synthExpr :: Ctx -> Ast -> DebugOr (Expr, QType)
synthExpr ctx p = case p of
  {-ALitBool b  ->
    return (ELitBool b, Unquantified CTBool)
  ALitInt i   ->
    return (ELitInt i, Unquantified CTI32) -}
  AName px -> do
    viable_var <- lookupVar (head px) ctx
    requireSingleton viable_var p
  AAbs bindings p' ->
    let
      (ps, pts) = unzip bindings
      vars      = map toExprName ps
    in do
      ts      <- checkUnquantTypes ctx pts
      (e, t2) <- synthExpr (extendVars (zip3 vars (map Unquantified ts) (repeat Nothing)) ctx) p'
      t2'     <- requireUnquantifiedType t2
      return (EAbs (zip vars ts) e, Unquantified $ CTArrow ts t2')
  AApp p1 p2 -> do
    viable_func <- resolve ctx p1 p2
    requireSingleton viable_func p
  AIf p1 p2 p3 -> do
    (e1, _)  <- checkExpr ctx p1 (Unquantified CTBool)
    (e2, t2) <- synthExpr ctx p2
    (e3, t3) <- synthExpr ctx p3
    requireOrElse (areStructurallyEqualQType t2 t3) "type mismatch"
    return (EIf e1 e2 e3, t2)
  _ -> fail $ "expected an expression to synthesize; got a " ++ summerizeForm p

requireTypeEq :: QType -> QType -> DebugOr ()
requireTypeEq t u = if areStructurallyEqualQType t u
  then return ()
  else fail $ "type `" ++ show t ++ "` is not equal to `" ++ show u ++ "`"

fitsIn32 :: Integer -> Bool
fitsIn32 i = i > 2147483647 || i < -2147483648

interpret_literal_integer :: Integer -> QType -> DebugOr Expr
interpret_literal_integer i qt = case qt of
  (Unquantified CTI32) -> if fitsIn32 i
    then mkSuccess $ ELitInt $ fromInteger i
    else fail $ "integer `" ++ show i ++ "` cannot fit into an unsigned 32-bit representation"
  _ -> fail $ "cannot convert integer `" ++ show i ++ "` to type `" ++ show qt ++ "`"

interpret_literal_double _ _ = fail "unsupported"

-- Derive a type checking judgment.
checkExpr :: Ctx -> Ast -> QType -> DebugOr (Expr, QType)
checkExpr ctx p ret_t = case p of
  ALitBoolean b -> do
    requireTypeEq ret_t (Unquantified CTBool)
    return (ELitBool b, Unquantified CTBool)
  ALitInteger i -> do
    e <- interpret_literal_integer i ret_t
    return (e, ret_t)
  ALitDouble d -> do
    _ <- interpret_literal_double d ret_t
    fail "unsupported"
  AName px -> do
    viable_func <- lookupVar (head px) ctx
    selectByType ret_t viable_func
  AAbs bindings p' ->
    let
      (ns, nts) = unzip bindings
      vars      = map toExprName ns
    in do
      (src_ts, tgt_t) <- requireArrowType ret_t
      src_ts'         <- checkUnquantTypes ctx nts
      requireTypeEqs src_ts src_ts'
      (e, tgt_t')     <- checkExpr (extendVars (zip3 vars (map Unquantified src_ts') (repeat Nothing)) ctx) p' (Unquantified tgt_t)
      requireOrElse (areStructurallyEqualQType tgt_t' (Unquantified tgt_t)) "type mismatch: unimplemented"
      return (EAbs (zip vars src_ts) e, ret_t)
  AApp p1 p2 -> do
    viable_func <- resolve ctx p1 p2
    selectByType ret_t viable_func
  AIf p1 p2 p3 -> do
    (e1, _)  <- checkExpr ctx p1 (Unquantified CTBool)
    (e2, _) <- checkExpr ctx p2 ret_t
    (e3, _) <- checkExpr ctx p3 ret_t
    return (EIf e1 e2 e3, ret_t)
  _ -> fail $ "expected an expression to check; got a " ++ summerizeForm p

checkExprs :: Ctx -> [(Ast, QType)] -> DebugOr [(Expr, QType)]
checkExprs ctx = traverse (uncurry $ checkExpr ctx)

-- | Requires that `x:t` is not defined.
requireNotDefined :: Ctx -> ExprName -> QType -> DebugOr ()
requireNotDefined ctx x t = if fromDebugOr (lookupSignature ctx x t) isDef (const False)
  then fail $ "attempting to redefine " ++ show (x,t)
  else mkSuccess ()

-- | Type check a top-level declaration or definition.
-- FIXME: Why does this not return a binding?
checkTopLevelBinding :: Ctx -> Ast -> DebugOr (ExprName, QType, Expr)
checkTopLevelBinding ctx p = case p of
  {-ADef x_p t_p e_p -> do
    t <- checkType ctx t_p
    let x = toExprName x_p
    _ <- requireNotDefined ctx x t
    (e, _) <- checkExpr ctx e_p t
    return (x, t, e)-}
  _ -> fail $ "expected a binding; got " ++ show p


