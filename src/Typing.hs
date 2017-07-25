module Typing ( synthExpr
              , checkExpr
              , checkType) where

--  TODOS
--    Add better debug support. Need locus information from the parser.

import Parser (Ast(..), AstName(AstName))
import Util.DebugOr (
  DebugOr(DebugOr, debugRep),
  onlySuccessful,
  requireOrElse )
import Expressions ( Expr(..)
                   , CType(..)
                   , QType(..)
                   , ExprName(..)
                   , TypeName(..)
                   , substExpr
                   , substExprs
                   , areStructurallyEqualCType
                   , areStructurallyEqualQType
                   )
import Contexts ( Ctx(..), Binding(BVar), extendVars )

import Data.Maybe (isNothing, catMaybes)


--------------------------------------------------------------------------------
-- Helper functions.

toExprName :: AstName -> ExprName
toExprName (AstName n) = ExprName n

toTypeName :: AstName -> TypeName
toTypeName (AstName n) = TypeName n



--------------------------------------------------------------------------------
--  Contexts.



newtype OverloadSet = OverloadSet { interps :: [(Expr, QType)] }

-- Print just the immediate node.
summerizeForm :: Ast -> String
summerizeForm ast = case ast of
  A_lit_bool b -> show b
  A_lit_int n  -> show n
  A_arrow _ _  -> "_ -> _"
  A_name _     -> "x"
  A_abs _ _    -> "\\(x:t,...) -> e"
  A_app _ _    -> "e e"
  A_if _ _ _   -> "if b then e else e"
  A_def _ _    -> "def x:t := e"



--------------------------------------------------------------------------------
-- Requirement combinators.

requireSingleton :: OverloadSet -> Ast -> DebugOr (Expr, QType)
requireSingleton ovlds ast = case interps ovlds of
  []     -> fail $ "no viable interpretation of " ++ summerizeForm ast
  v : [] -> return v
  _      -> fail $ "ambiguous interpretation for " ++ summerizeForm ast

requireUnquantifiedType :: QType -> DebugOr CType
requireUnquantifiedType qt = case qt of
  Quantified _ _ _ -> fail "cannot convert type to unquantified"
  Unquantified t   -> return t

requireArity :: [a] -> [b] -> DebugOr ()
requireArity xs ys =
  if length xs == length ys then return () else fail "different lengths"

requireUnquantifiedTypes :: [QType] -> DebugOr [CType]
requireUnquantifiedTypes = traverse requireUnquantifiedType

requireArrowType :: QType -> DebugOr ([CType], CType)
requireArrowType qt = case qt of
  Unquantified (CT_arrow t1 t2) -> return (t1, t2)
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
    return (E_app f args, Unquantified tgt_t)

-- Perform C++-style overload resolution for the call `p ps`.
resolve :: Ctx -> Ast -> [Ast] -> DebugOr OverloadSet
resolve ctx p ps = case p of
  A_name n -> do
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
    var_binding_has_name v b = case b of BVar v' t -> v == v'
    overloads = filter (var_binding_has_name $ ExprName n) bindings
  in DebugOr $ Right $ OverloadSet $
    map (\(BVar x t) -> (E_var x t, t)) overloads

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
  A_lit_bool _  ->
    fail $ "expected type; got " ++ summerizeForm p
  A_lit_int _   ->
    fail $ "expected type; got " ++ summerizeForm p
  A_arrow ps p -> do
    src_ts <- checkUnquantTypes ctx ps
    tgt_t  <- checkUnquantType ctx p
    return $ CT_arrow src_ts tgt_t
  A_name xt ->
    let (AstName n) = xt in
      return $ CT_var $ TypeName n
  A_abs _ _ ->
    fail $ "expected type; got " ++ summerizeForm p
  A_app _ _ ->
    fail $ "expected type; got " ++ summerizeForm p
  A_if _ _ _ ->
    fail $ "expected type; got " ++ summerizeForm p
  A_def _ _ ->
    fail $ "expected type; got " ++ summerizeForm p

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
  A_lit_bool b  ->
    return (E_lit_bool b, Unquantified CT_bool)
  A_lit_int i   ->
    return (E_lit_int i, Unquantified CT_int)
  A_name px -> do
    viable <- lookupVar px ctx
    requireSingleton viable p
  A_abs bindings p ->
    let
      (ps, pts) = unzip bindings
      vars      = map toExprName ps
    in do
      ts      <- checkUnquantTypes ctx pts
      (e, t2) <- synthExpr (extendVars (zip vars $ map Unquantified ts) ctx) p
      t2'     <- requireUnquantifiedType t2
      return (E_abs (zip vars ts) e, Unquantified $ CT_arrow ts t2')
  A_app p1 p2 -> do
    viable <- resolve ctx p1 p2
    requireSingleton viable p
  A_if p1 p2 p3 -> do
    (e1, _)  <- checkExpr ctx p1 (Unquantified CT_bool)
    (e2, t2) <- synthExpr ctx p2
    (e3, t3) <- synthExpr ctx p3
    requireOrElse (areStructurallyEqualQType t2 t3) "type mismatch"
    return (E_if e1 e2 e3, t2)
  _ -> fail $ "expected an expression to synthesize; got a " ++ summerizeForm p

synthExprs :: Ctx -> [Ast] -> DebugOr [(Expr, QType)]
synthExprs ctx = traverse (synthExpr ctx)

-- Derive a type checking judgment.
checkExpr :: Ctx -> Ast -> QType -> DebugOr (Expr, QType)
checkExpr ctx p ret_t = case p of
  A_lit_bool b  -> do
    requireOrElse (areStructurallyEqualQType ret_t (Unquantified CT_bool)) "need better err msg"
    return (E_lit_bool b, Unquantified CT_bool)
  A_lit_int i   -> do
    requireOrElse (areStructurallyEqualQType ret_t (Unquantified CT_int)) "need better err msg"
    return (E_lit_int i, Unquantified CT_int)
  A_name px -> do
    viable <- lookupVar px ctx
    selectByType ret_t viable
  A_abs bindings p ->
    let
      (ns, nts) = unzip bindings
      vars      = map toExprName ns
    in do
      (src_ts, tgt_t) <- requireArrowType ret_t
      src_ts'         <- checkUnquantTypes ctx nts
      requireTypeEqs src_ts src_ts'
      (e, tgt_t')     <- checkExpr (extendVars (zip vars $ map Unquantified src_ts') ctx) p (Unquantified tgt_t)
      requireOrElse (areStructurallyEqualQType tgt_t' (Unquantified tgt_t)) "type mismatch: unimplemented"
      return (E_abs (zip vars src_ts) e, ret_t)
  A_app p1 p2 -> do
    viable <- resolve ctx p1 p2
    selectByType ret_t viable
  A_if p1 p2 p3 -> do
    (e1, _)  <- checkExpr ctx p1 (Unquantified CT_bool)
    (e2, _) <- checkExpr ctx p2 ret_t
    (e3, _) <- checkExpr ctx p3 ret_t
    return (E_if e1 e2 e3, ret_t)
  _ -> fail $ "expected an expression to check; got a " ++ summerizeForm p

checkExprs :: Ctx -> [(Ast, QType)] -> DebugOr [(Expr, QType)]
checkExprs ctx = traverse (uncurry $ checkExpr ctx)
