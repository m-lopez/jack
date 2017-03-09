module Typing (Expr, synth_expr, check_type, eval, init_ctx, DebugOr, debug_rep) where

--  TODOS
--    Add better debug support. Need locus information from the parser.

import Parser (Ast(..), Ast_name(Ast_name))

import Data.Either (lefts, rights)
import Data.List (intercalate)
import Data.Maybe (isNothing, catMaybes)


--------------------------------------------------------------------------------
--  Ground values.
--
--  Its a lot of extra boilerplate, but it keeps things safe.

newtype Expr_name = Expr_name String deriving (Show, Eq)

to_expr_name :: Ast_name -> Expr_name
to_expr_name (Ast_name n) = Expr_name n

newtype Type_name = Type_name String deriving (Show, Eq)

to_type_name :: Ast_name -> Type_name
to_type_name (Ast_name n) = Type_name n

--------------------------------------------------------------------------------
--  Types

-- Check back!
data Prop = P_true deriving (Show)

data QType =
    Quantified   [Type_name] Prop CType
  | Unquantified CType
  deriving (Show)

data CType =
    CT_bool
  | CT_int
  | CT_arrow [CType] CType
  | CT_var   Type_name
  deriving (Show)



--------------------------------------------------------------------------------
--  Expressions

data Expr =
    E_lit_bool Bool
  | E_lit_int  Integer
  | E_var      Expr_name QType
  | E_abs      [(Expr_name, CType)] Expr
  | E_app      Expr [Expr]
  | E_if       Expr Expr Expr
  deriving (Show)



--------------------------------------------------------------------------------
--  Contexts.

data Binding = B_var Expr_name QType

newtype Ctx = Ctx [Binding]

init_ctx :: Ctx
init_ctx = Ctx []

extend_var :: Expr_name -> QType -> Ctx -> Ctx
extend_var x t (Ctx ctx) = Ctx $ (B_var x t) : ctx

extend_vars :: [(Expr_name, QType)] -> Ctx -> Ctx
extend_vars xts (Ctx ctx) = Ctx (new_bindings ++ ctx)
  where
    new_bindings = map (uncurry B_var) xts

newtype OverloadSet = OverloadSet { interps :: [(Expr, QType)] }



--------------------------------------------------------------------------------
--  Type checking monad.

newtype DebugOr a = DebugOr { debug_rep :: Either String a}

instance Functor DebugOr where
  fmap f dx = let DebugOr x = dx in case x of
    Left err -> DebugOr $ Left err
    Right x' -> DebugOr $ Right $ f x'

instance Applicative DebugOr where
  pure x = DebugOr $ Right x
  f <*> x =
    let
      DebugOr f' = f
      DebugOr x' = x
    in case (f', x') of
      (Right f'', Right x'') -> DebugOr $ Right $ f'' x''
      (Left err, _)          -> DebugOr $ Left err
      (_, Left err)          -> DebugOr $ Left err

-- FIXME: Should the context be threaded through this?
instance Monad DebugOr where
  return x = DebugOr $ Right x
  x >>= f = let DebugOr x' = x in case x' of
    Left msg -> fail msg
    Right y  -> f y
  fail msg = DebugOr $ Left msg

-- Report all debugging info. If there is none, return all values.
-- FIXME: Use `sequence`! Why roll your own?
all_or_nothing :: [DebugOr a] -> DebugOr [a]
all_or_nothing dbgs =
  let
    eithers = map debug_rep dbgs
    errs    = lefts eithers
    as      = rights eithers
  in case errs of
    [] -> DebugOr $ Right as
    _  -> DebugOr $ Left $ intercalate " | " errs

-- Discard debug errors.
only_successful :: [DebugOr a] -> DebugOr [a]
only_successful dbgs = DebugOr $ Right $ rights $ fmap debug_rep dbgs

-- Same as above for Maybe. I smell an abstraction.
all_just_or_nothing :: [Maybe a] -> Maybe [a]
all_just_or_nothing maybes =
  if any isNothing maybes
    then Nothing
    else Just $ catMaybes maybes

-- Print just the immediate node.
summerize_form :: Ast -> String
summerize_form ast = case ast of
  A_lit_bool b -> show b
  A_lit_int n  -> show n
  A_arrow _ _  -> "_ -> _"
  A_name _     -> "variable"
  A_abs _ _    -> "\\(x:t,...) -> e"
  A_app _ _    -> "e e"
  A_if _ _ _   -> "if b then e else e"
  A_coerce _ _ -> "e @ t"
  A_def _ _    -> "def x:t := e"



--------------------------------------------------------------------------------
-- Requirement combinators.

require_or_else :: Bool -> String -> DebugOr ()
require_or_else b msg = case b of
  True  -> return ()
  False -> fail msg

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
require_unquantified_types qts =
  all_or_nothing $ map require_unquantified_type qts

require_arrow_type :: QType -> DebugOr ([CType], CType)
require_arrow_type qt = case qt of
  Unquantified (CT_arrow t1 t2) -> return (t1, t2)
  _ -> fail $ "expected an arrow type; got a " ++ show qt

require_type_eqs :: [CType] -> [CType] -> DebugOr ()
require_type_eqs ts us = case length ts == length us of
  False -> fail $ "types not equal: unimplemented"
  True  -> case all (uncurry type_equiv_ctype) $ zip ts us of
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



--------------------------------------------------------------------------------
--  Substitution system.

subst_expr :: Expr_name -> QType -> Expr -> Expr -> Expr
subst_expr x t v e =
  let
    subst_on = subst_expr x t v  -- subst on, wayne! subst on, garth!
  in case e of
    E_lit_bool _      -> e
    E_lit_int  _      -> e
    E_var y t'        -> if (x == y) && (type_equiv t t') then v else e
    E_abs params body -> if any is_x_t params
                           then e
                           else E_abs params $ subst_on body
      where
        is_x_t (y,t') = (x == y) && (type_equiv t $ Unquantified t')
    E_app e1 args     -> E_app (subst_on e1) (map subst_on args)
    E_if c e1 e2      -> E_if (subst_on c) (subst_on e1) (subst_on e2)

subst_exprs :: [((Expr_name, QType), Expr)] -> Expr -> Expr
subst_exprs param_map e = all_substs e
  where
    subst_ons  = map (uncurry $ uncurry subst_expr) param_map
    all_substs = foldr (.) id subst_ons



--------------------------------------------------------------------------------
--  Unification systems.
--
--  Systems for unifying types and terms.

-- Type equality on quantified types.
type_equiv :: QType -> QType -> Bool
type_equiv t1 t2 = case (t1,t2) of
  (Unquantified t1', Unquantified t2') -> type_equiv_ctype t1' t2'
  _                                    -> False

-- Type equality on unquantified types.
type_equiv_ctype :: CType -> CType -> Bool
type_equiv_ctype t1 t2 = case (t1,t2) of
  (CT_bool, CT_bool)         -> True
  (CT_int,  CT_int)          -> True
  (CT_arrow src_t1 tgt_t1, CT_arrow src_t2 tgt_t2) ->
    let
      same_lengths = length src_t1 == length src_t2
      sources_eq   = all (uncurry type_equiv_ctype) (zip src_t1 src_t2)
      targets_eq   = type_equiv_ctype tgt_t1 tgt_t2
    in same_lengths && sources_eq && targets_eq
  (CT_var (Type_name x1), CT_var (Type_name x2)) -> x1 == x2



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
  case filter (type_equiv t . snd) ovld of
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
  A_coerce _ _ ->
    fail $ "expected type; got " ++ summerize_form p
  A_def _ _ ->
    fail $ "expected type; got " ++ summerize_form p

check_unquant_types :: Ctx -> [Ast] -> DebugOr [CType]
check_unquant_types ctx pts = all_or_nothing $ map (check_unquant_type ctx) pts



--------------------------------------------------------------------------------
--  Type checking.
--
--  A system for ascribing types to programs.

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
    require_or_else (type_equiv t2 t3) "type mismatch"
    return (E_if e1 e2 e3, t2)
  A_coerce p tp -> do
    t      <- check_type ctx tp
    (e, _) <- check_expr ctx p t
    return (e, t)
  _ -> fail $ "expected an expression to synthesize; got a " ++ summerize_form p

synth_exprs :: Ctx -> [Ast] -> DebugOr [(Expr, QType)]
synth_exprs ctx ps = all_or_nothing $ map (synth_expr ctx) ps

-- Derive a type checking judgment.
check_expr :: Ctx -> Ast -> QType -> DebugOr (Expr, QType)
check_expr ctx p ret_t = case p of
  A_lit_bool b  -> do
    require_or_else (type_equiv ret_t (Unquantified CT_bool)) "need better err msg"
    return (E_lit_bool b, Unquantified CT_bool)
  A_lit_int i   -> do
    require_or_else (type_equiv ret_t (Unquantified CT_int)) "need better err msg"
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
      require_or_else (type_equiv tgt_t' (Unquantified tgt_t)) "type mismatch: unimplemented"
      return (E_abs (zip vars src_ts) e, ret_t)
  A_app p1 p2 -> do
    viable <- resolve ctx p1 p2
    select_by_type ret_t viable
  A_if p1 p2 p3 -> do
    (e1, _)  <- check_expr ctx p1 (Unquantified CT_bool)
    (e2, _) <- check_expr ctx p2 ret_t
    (e3, _) <- check_expr ctx p3 ret_t
    return (E_if e1 e2 e3, ret_t)
  A_coerce p tp -> do
    t      <- check_type ctx tp
    (e, _) <- check_expr ctx p t
    require_or_else (type_equiv t ret_t) "need a better error message"
    return (e, ret_t)
  _ -> fail $ "expected an expression to check; got a " ++ summerize_form p

check_exprs :: Ctx -> [(Ast, QType)] -> DebugOr [(Expr, QType)]
check_exprs ctx pts = all_or_nothing $ map (\(p,t) -> check_expr ctx p t) pts



--------------------------------------------------------------------------------
--  Evaluation system

-- We allow Maybe to catch compiler errors that lead to an unsound type system.
eval :: Expr -> DebugOr Expr
eval e = case e of
  E_lit_bool _ ->
    return e
  E_lit_int _ ->
    return e
  E_var _ _ ->
    return e
  E_abs _ _ ->
    return e
  E_app e es -> do
    vs           <- evals es
    f            <- eval e
    (params, e') <- as_lambda f
    qparams      <- return $ map (\(x,t) -> (x, Unquantified t))  params
    require_or_else (length vs == length qparams) "arity doesn't match number of arguments"
    param_map    <- return $ zip qparams vs
    eval $ subst_exprs param_map e'
  E_if c e1 e2 -> do
    res <- eval c
    b   <- as_bool res
    if b then eval e1 else eval e2

evals :: [Expr] -> DebugOr [Expr]
evals es = all_or_nothing $ fmap eval es

as_lambda :: Expr -> DebugOr ([(Expr_name, CType)], Expr)
as_lambda e = case e of
  E_abs params e' -> return (params, e')
  _               -> fail "callee is not callable"

as_bool :: Expr -> DebugOr Bool
as_bool e = case e of
  E_lit_bool b -> return b
  _            -> fail "value is not a Boolean"