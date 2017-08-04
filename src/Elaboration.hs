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
  checkTopLevel ) where

--  TODOS
--    Add better debug support. Need locus information from the parser.

import Parser (Ast(..), AstName(AstName))
import Util.DebugOr (
  DebugOr(DebugOr),
  onlySuccessful,
  requireOrElse,
  mkSuccess )
import Expressions (
  Expr(..),
  CType(..),
  QType(..),
  ExprName(..),
  TypeName(..),
  areStructurallyEqualCType,
  areStructurallyEqualQType )
import Context ( Ctx(..), Binding(BVar), extendVars, lookupSignature )



-- | A helper function to convert AST symbols to expression symbols.
toExprName :: AstName -> ExprName
toExprName (AstName n) = ExprName n

-- | The type of an overload set.
newtype OverloadSet = OverloadSet { interps :: [(Expr, QType)] }

-- Print just the immediate node.
summerizeForm :: Ast -> String
summerizeForm ast = case ast of
  ALitBool b -> show b
  ALitInt n  -> show n
  AArrow _ _  -> "_ -> _"
  AName (AstName s) -> "`" ++ s ++ "`"
  AAbs _ _    -> "\\(x:t,...) -> e"
  AApp _ _    -> "e e"
  AIf _ _ _   -> "if b then e else e"
  ADef _ _ _  -> "def x:t := e"
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
    cands   <- lookupVar n ctx                          -- Computes a candidate set
    viables <- onlySuccessful $ map (\(f, t) -> viable ctx f t ps) $ interps cands  -- Computes viable functions
    _       <- requireOrElse (not $ null viables) "no valid interpretations for application"
    return $ OverloadSet viables
  -- FIXME: Should support lambdas.
  ast -> fail $ "attempted overload resolution on an uncallable expression; got " ++ show ast







--------------------------------------------------------------------------------
--  Lookup and selection system.
--
--  This system is responsible for querying the context for bindings.

-- Lookup an expression variable.
lookupVar :: AstName -> Ctx -> DebugOr OverloadSet
lookupVar (AstName n) (Ctx bindings) =
  let
    var_binding_has_name v b = case b of BVar v' _ _ -> v == v'
    overloads = filter (var_binding_has_name $ ExprName n) bindings
  in DebugOr $ Right $ OverloadSet $
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
checkType ctx p = let DebugOr k = checkUnquantType ctx p in case k of
  Left err -> DebugOr $ Left err
  Right t  -> DebugOr $ Right $ Unquantified t

checkUnquantType :: Ctx -> Ast -> DebugOr CType
checkUnquantType ctx p = case p of
  ATypeBool -> mkSuccess CTBool
  ATypeInt -> mkSuccess CTInt
  AArrow ps p' -> do
    src_ts <- checkUnquantTypes ctx ps
    tgt_t  <- checkUnquantType ctx p'
    return $ CTArrow src_ts tgt_t
  ARecT _ -> fail "sorry, support for record types has not been implemented"
  ALitBool _ -> fail_here
  ALitInt _   -> fail_here
  AName xt -> let (AstName n) = xt in return $ CTVar $ TypeName n
  AAbs _ _ -> fail_here
  AApp _ _ -> fail_here
  AIf _ _ _ -> fail_here
  ACoerce _ _ -> fail_here
  AInit _ -> fail_here
  ADef _ _ _ -> fail_here
  where fail_here = fail $ "expected unquantified type; got " ++ summerizeForm p

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
  ALitBool b  ->
    return (ELitBool b, Unquantified CTBool)
  ALitInt i   ->
    return (ELitInt i, Unquantified CTInt)
  AName px -> do
    viable_var <- lookupVar px ctx
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

-- synthExprs :: Ctx -> [Ast] -> DebugOr [(Expr, QType)]
-- synthExprs ctx = traverse (synthExpr ctx)

-- Derive a type checking judgment.
checkExpr :: Ctx -> Ast -> QType -> DebugOr (Expr, QType)
checkExpr ctx p ret_t = case p of
  ALitBool b  -> do
    requireOrElse (areStructurallyEqualQType ret_t (Unquantified CTBool)) "need better err msg"
    return (ELitBool b, Unquantified CTBool)
  ALitInt i   -> do
    requireOrElse (areStructurallyEqualQType ret_t (Unquantified CTInt)) "need better err msg"
    return (ELitInt i, Unquantified CTInt)
  AName px -> do
    viable_func <- lookupVar px ctx
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
requireNotDefined ctx x t = let
    b = lookupSignature ctx x t
  in case b of
    DebugOr (Right (BVar _ _ (Just _))) -> fail $ "attempting to redefine " ++ show (x,t)
    _ -> mkSuccess ()

-- | FIXME: Ad-hoc. No thought went into this.
checkTopLevel :: Ctx -> Ast -> DebugOr (ExprName, QType, Expr)
checkTopLevel ctx (ADef x_p t_p e_p) = do
  t <- checkType ctx t_p
  let x = toExprName x_p
  _ <- requireNotDefined ctx x t
  (e, _) <- checkExpr ctx e_p t
  return (x, t, e)
checkTopLevel _ p = fail $ "expected a definition; got " ++ show p