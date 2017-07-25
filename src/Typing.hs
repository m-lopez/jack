module Typing ( synth_expr
              , check_expr
              , check_type) where

--  TODOS
--    Add better debug support. Need locus information from the parser.

import Parser (Ast(..), Ast_name(Ast_name))
import Util.DebugOr (
  DebugOr(DebugOr, debug_rep),
  only_successful,
  require_or_else )
import Expressions (Expr(..)
                   , CType(..)
                   , QType(..)
                   , Expr_name(..)
                   , Type_name(..)
                   , subst_expr
                   , subst_exprs
                   , are_structurally_equal_ctype
                   , are_structurally_equal_qtype
                   )
import Contexts ( Ctx(..), Binding(B_var), extend_vars )

import Data.Maybe (isNothing, catMaybes)


--------------------------------------------------------------------------------
-- Helper functions.

to_expr_name :: Ast_name -> Expr_name
to_expr_name (Ast_name n) = Expr_name n

to_type_name :: Ast_name -> Type_name
to_type_name (Ast_name n) = Type_name n



--------------------------------------------------------------------------------
--  Contexts.



newtype OverloadSet = OverloadSet { interps :: [(Expr, QType)] }

-- Print just the immediate node.
summerize_form :: Ast -> String
summerize_form ast = case ast of
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

require_singleton :: OverloadSet -> Ast -> DebugOr (Expr, QType)
require_singleton ovlds ast = case interps ovlds of
  []     -> fail $ "no viable interpretation of " ++ summerize_form ast
  v : [] -> return v
  _      -> fail $ "ambiguous interpretation for " ++ summerize_form ast

require_unquantified_type :: QType -> DebugOr CType
require_unquantified_type qt = case qt of
  Quantified _ _ _ -> fail "cannot convert type to unquantified"
  Unquantified t   -> return t

require_arity :: [a] -> [b] -> DebugOr ()
require_arity xs ys =
  if (length xs) == (length ys) then return () else fail "different lengths"

require_unquantified_types :: [QType] -> DebugOr [CType]
require_unquantified_types qts = traverse require_unquantified_type qts

require_arrow_type :: QType -> DebugOr ([CType], CType)
require_arrow_type qt = case qt of
  Unquantified (CT_arrow t1 t2) -> return (t1, t2)
  _ -> fail $ "expected an arrow type; got a " ++ show qt

require_type_eqs :: [CType] -> [CType] -> DebugOr ()
require_type_eqs ts us = case length ts == length us of
  False -> fail $ "types not equal: unimplemented"
  True  -> case all (uncurry are_structurally_equal_ctype) $ zip ts us of
    False -> fail $ "type mismatch: unimplmented"
    True  -> return ()



--------------------------------------------------------------------------------
--  Overload resolution.

-- Attempt to type check `f:t` applied to the arguments `ps` with respect to the
-- context `ctx`.
viable :: Ctx -> Expr -> QType -> [Ast] -> DebugOr (Expr, QType)
viable ctx f t ps = do
  (src_ts, tgt_t) <- require_arrow_type t
  _               <- require_arity ps src_ts
  typed_args      <- check_exprs ctx $ zip ps $ fmap Unquantified src_ts
  let args = fst $ unzip typed_args in
    return (E_app f args, Unquantified tgt_t)

-- Perform C++-style overload resolution for the call `p ps`.
resolve :: Ctx -> Ast -> [Ast] -> DebugOr OverloadSet
resolve ctx p ps = case p of
  A_name n -> do
    cands   <- lookup_var n ctx                          -- Computes a candidate set
    viables <- only_successful $ map (\(f, t) -> viable ctx f t ps) $ interps cands  -- Computes viable functions
    _       <- require_or_else (not $ null viables) "no valid interpretations for application"
    return $ OverloadSet viables
  -- FIXME: Should support lambdas.
  ast -> fail $ "attempted overload resolution on an uncallable expression; got " ++ show ast







--------------------------------------------------------------------------------
--  Lookup and selection system.
--
--  This system is responsible for querying the context for bindings.

-- Lookup an expression variable.
lookup_var :: Ast_name -> Ctx -> DebugOr OverloadSet
lookup_var (Ast_name n) (Ctx bindings) =
  let
    var_binding_has_name v b = case b of B_var v' t -> v == v'
    overloads = filter (var_binding_has_name $ Expr_name n) bindings
  in DebugOr $ Right $ OverloadSet $
    map (\(B_var x t) -> (E_var x t, t)) overloads

-- Select from an overload on type.
select_by_type :: QType -> OverloadSet -> DebugOr (Expr, QType)
select_by_type t (OverloadSet ovld) =
  case filter (are_structurally_equal_qtype t . snd) ovld of
    []     -> fail $ "no viable overload matches the type " ++ show t
    x : [] -> return x
    _      -> fail $ "ambiguous selection"



--------------------------------------------------------------------------------
--  Kind checking.

check_type :: Ctx -> Ast -> DebugOr QType
check_type ctx p = let DebugOr k = check_unquant_type ctx p in case k of
  Left err -> DebugOr $ Left err
  Right t  -> DebugOr $ Right $ Unquantified t

check_unquant_type :: Ctx -> Ast -> DebugOr CType
check_unquant_type ctx p = case p of
  A_lit_bool _  ->
    fail $ "expected type; got " ++ summerize_form p
  A_lit_int _   ->
    fail $ "expected type; got " ++ summerize_form p
  A_arrow ps p -> do
    src_ts <- check_unquant_types ctx ps
    tgt_t  <- check_unquant_type ctx p
    return $ CT_arrow src_ts tgt_t
  A_name xt ->
    let (Ast_name n) = xt in
      return $ CT_var $ Type_name n
  A_abs _ _ ->
    fail $ "expected type; got " ++ summerize_form p
  A_app _ _ ->
    fail $ "expected type; got " ++ summerize_form p
  A_if _ _ _ ->
    fail $ "expected type; got " ++ summerize_form p
  A_def _ _ ->
    fail $ "expected type; got " ++ summerize_form p

check_unquant_types :: Ctx -> [Ast] -> DebugOr [CType]
check_unquant_types ctx pts = traverse (check_unquant_type ctx) pts



--------------------------------------------------------------------------------
--  Type checking.
--
--  This is a system for transforming untyped ASTs to typed elaborations.
--
--  This language uses a bidirectional type system [Pierce reference.] Type
--  elaboration is separated into type checking rules `check_expr` and
--  respective type inference rules `synth_expr`.

-- Synthesize a typed expression from an AST.
synth_expr :: Ctx -> Ast -> DebugOr (Expr, QType)
synth_expr ctx p = case p of
  A_lit_bool b  ->
    return (E_lit_bool b, Unquantified CT_bool)
  A_lit_int i   ->
    return (E_lit_int i, Unquantified CT_int)
  A_name px -> do
    viable <- lookup_var px ctx
    require_singleton viable p
  A_abs bindings p ->
    let
      (ps, pts) = unzip bindings
      vars      = map to_expr_name ps
    in do
      ts      <- check_unquant_types ctx pts
      (e, t2) <- synth_expr (extend_vars (zip vars $ map Unquantified ts) ctx) p
      t2'     <- require_unquantified_type t2
      return (E_abs (zip vars ts) e, Unquantified $ CT_arrow ts t2')
  A_app p1 p2 -> do
    viable <- resolve ctx p1 p2
    require_singleton viable p
  A_if p1 p2 p3 -> do
    (e1, _)  <- check_expr ctx p1 (Unquantified CT_bool)
    (e2, t2) <- synth_expr ctx p2
    (e3, t3) <- synth_expr ctx p3
    require_or_else (are_structurally_equal_qtype t2 t3) "type mismatch"
    return (E_if e1 e2 e3, t2)
  _ -> fail $ "expected an expression to synthesize; got a " ++ summerize_form p

synth_exprs :: Ctx -> [Ast] -> DebugOr [(Expr, QType)]
synth_exprs ctx ps = traverse (synth_expr ctx) ps

-- Derive a type checking judgment.
check_expr :: Ctx -> Ast -> QType -> DebugOr (Expr, QType)
check_expr ctx p ret_t = case p of
  A_lit_bool b  -> do
    require_or_else (are_structurally_equal_qtype ret_t (Unquantified CT_bool)) "need better err msg"
    return (E_lit_bool b, Unquantified CT_bool)
  A_lit_int i   -> do
    require_or_else (are_structurally_equal_qtype ret_t (Unquantified CT_int)) "need better err msg"
    return (E_lit_int i, Unquantified CT_int)
  A_name px -> do
    viable <- lookup_var px ctx
    select_by_type ret_t viable
  A_abs bindings p ->
    let
      (ns, nts) = unzip bindings
      vars      = map to_expr_name ns
    in do
      (src_ts, tgt_t) <- require_arrow_type ret_t
      src_ts'         <- check_unquant_types ctx nts
      require_type_eqs src_ts src_ts'
      (e, tgt_t')     <- check_expr (extend_vars (zip vars $ map Unquantified src_ts') ctx) p (Unquantified tgt_t)
      require_or_else (are_structurally_equal_qtype tgt_t' (Unquantified tgt_t)) "type mismatch: unimplemented"
      return (E_abs (zip vars src_ts) e, ret_t)
  A_app p1 p2 -> do
    viable <- resolve ctx p1 p2
    select_by_type ret_t viable
  A_if p1 p2 p3 -> do
    (e1, _)  <- check_expr ctx p1 (Unquantified CT_bool)
    (e2, _) <- check_expr ctx p2 ret_t
    (e3, _) <- check_expr ctx p3 ret_t
    return (E_if e1 e2 e3, ret_t)
  _ -> fail $ "expected an expression to check; got a " ++ summerize_form p

check_exprs :: Ctx -> [(Ast, QType)] -> DebugOr [(Expr, QType)]
check_exprs ctx pts = traverse (uncurry $ check_expr ctx) pts
