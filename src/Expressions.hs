-- This module defines the expression languages and fundamental expression
-- algorithms.

module Expressions( Expr(..)
                  , QType(..)
                  , CType(..)
                  , ExprName(..)
                  , TypeName(..)
                  , substExpr
                  , substExprs
                  , areStructurallyEqualExpr
                  , areStructurallyEqualCType
                  , areStructurallyEqualQType
                  ) where

import Parser (Ast(..), AstName(AstName))



--------------------------------------------------------------------------------
--  Symbol names for Expressions and types.

newtype ExprName = ExprName String deriving (Show, Eq)
newtype TypeName = TypeName String deriving (Show, Eq)



-------------------------------------
-- Expression syntax
--

data Expr =
    E_lit_bool Bool
  | E_lit_int  Integer
  | E_var      ExprName QType
  | E_abs      [(ExprName, CType)] Expr
  | E_app      Expr [Expr]
  | E_if       Expr Expr Expr
  deriving (Show)

-------------------------------------
-- Proposition syntax
--

data Prop = P_true deriving (Show)

-------------------------------------
-- Type syntax
--

data QType =
    Quantified   [TypeName] Prop CType
  | Unquantified CType
  deriving (Show)

data CType =
    CT_bool
  | CT_int
  | CT_arrow [CType] CType
  | CT_var   TypeName
  deriving (Show)

--------------------------------------------------------------------------------
--  Substitution system.

substExpr :: ExprName -> QType -> Expr -> Expr -> Expr
substExpr x t v e =
  let
    subst_on = substExpr x t v  -- subst on, wayne! subst on, garth!
  in case e of
    E_lit_bool _      -> e
    E_lit_int  _      -> e
    E_var y t'        -> if (x == y) && areStructurallyEqualQType t t' then v else e
    E_abs params body -> if any is_x_t params
                           then e
                           else E_abs params $ subst_on body
      where
        is_x_t (y,t') = (x == y) && (areStructurallyEqualQType t $ Unquantified t')
    E_app e1 args     -> E_app (subst_on e1) (map subst_on args)
    E_if c e1 e2      -> E_if (subst_on c) (subst_on e1) (subst_on e2)

substExprs :: [((ExprName, QType), Expr)] -> Expr -> Expr
substExprs param_map = foldr (.) id subst_ons
  where
    subst_ons  = map (uncurry $ uncurry substExpr) param_map



--------------------------------------------------------------------------------
--  Unification systems.
--
--  Systems for unifying types and terms.

-- Type equality on quantified types.
areStructurallyEqualQType :: QType -> QType -> Bool
areStructurallyEqualQType t1 t2 = case (t1,t2) of
  (Unquantified t1', Unquantified t2') -> areStructurallyEqualCType t1' t2'
  _                                    -> False

-- Type equality on unquantified types.
areStructurallyEqualCType :: CType -> CType -> Bool
areStructurallyEqualCType t1 t2 = case (t1,t2) of
  (CT_bool, CT_bool)         -> True
  (CT_int,  CT_int)          -> True
  (CT_arrow src_t1 tgt_t1, CT_arrow src_t2 tgt_t2) ->
    let
      same_lengths = length src_t1 == length src_t2
      sources_eq   = all (uncurry areStructurallyEqualCType) (zip src_t1 src_t2)
      targets_eq   = areStructurallyEqualCType tgt_t1 tgt_t2
    in same_lengths && sources_eq && targets_eq
  (CT_var (TypeName x1), CT_var (TypeName x2)) -> x1 == x2
  _ -> False

-- Structural equality on expressions.
-- Note: This does not compute alpha equivalence.
areStructurallyEqualExpr :: Expr -> Expr -> Bool
areStructurallyEqualExpr x y = let
    are_bindings_eq (ExprName x, t) (ExprName x', t') = x == x' && areStructurallyEqualCType t t'
    are_list_bindings_eq bs bs' = (length bs == length bs') && all (uncurry are_bindings_eq) (zip bs bs')
  in case (x,y) of
    (E_lit_bool b, E_lit_bool b') -> b == b'
    (E_lit_int n, E_lit_int n') -> n == n'
    (E_var x t, E_var x' t') -> x == x' && areStructurallyEqualQType t t'
    (E_abs bs e, E_abs bs' e') ->
      are_list_bindings_eq bs bs' && areStructurallyEqualExpr e e'
    (E_app e es, E_app e' es') ->
      areStructurallyEqualExpr e e' && (length es == length es') &&
        all (uncurry areStructurallyEqualExpr) (zip es es')
    (E_if e1 e2 e3, E_if e1' e2' e3') ->
      areStructurallyEqualExpr e1 e1' &&
        areStructurallyEqualExpr e2 e2' &&
          areStructurallyEqualExpr e3 e3'
