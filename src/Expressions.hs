-- This module defines the expression languages and fundamental expression
-- algorithms.

module Expressions( Expr(..)
                  , QType(..)
                  , CType(..)
                  , Expr_name(..)
                  , Type_name(..)
                  , subst_expr
                  , subst_exprs
                  , are_structurally_equal_expr
                  , are_structurally_equal_ctype
                  , are_structurally_equal_qtype
                  ) where

import Parser (Ast(..), Ast_name(Ast_name))



--------------------------------------------------------------------------------
--  Symbol names for Expressions and types.

data Expr_name = Expr_name String deriving (Show, Eq)
newtype Type_name = Type_name String deriving (Show, Eq)



-------------------------------------
-- Expression syntax
--

data Expr =
    E_lit_bool Bool
  | E_lit_int  Integer
  | E_var      Expr_name QType
  | E_abs      [(Expr_name, CType)] Expr
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
--  Substitution system.

subst_expr :: Expr_name -> QType -> Expr -> Expr -> Expr
subst_expr x t v e =
  let
    subst_on = subst_expr x t v  -- subst on, wayne! subst on, garth!
  in case e of
    E_lit_bool _      -> e
    E_lit_int  _      -> e
    E_var y t'        -> if (x == y) && (are_structurally_equal_qtype t t') then v else e
    E_abs params body -> if any is_x_t params
                           then e
                           else E_abs params $ subst_on body
      where
        is_x_t (y,t') = (x == y) && (are_structurally_equal_qtype t $ Unquantified t')
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
are_structurally_equal_qtype :: QType -> QType -> Bool
are_structurally_equal_qtype t1 t2 = case (t1,t2) of
  (Unquantified t1', Unquantified t2') -> are_structurally_equal_ctype t1' t2'
  _                                    -> False

-- Type equality on unquantified types.
are_structurally_equal_ctype :: CType -> CType -> Bool
are_structurally_equal_ctype t1 t2 = case (t1,t2) of
  (CT_bool, CT_bool)         -> True
  (CT_int,  CT_int)          -> True
  (CT_arrow src_t1 tgt_t1, CT_arrow src_t2 tgt_t2) ->
    let
      same_lengths = length src_t1 == length src_t2
      sources_eq   = all (uncurry are_structurally_equal_ctype) (zip src_t1 src_t2)
      targets_eq   = are_structurally_equal_ctype tgt_t1 tgt_t2
    in same_lengths && sources_eq && targets_eq
  (CT_var (Type_name x1), CT_var (Type_name x2)) -> x1 == x2

-- Structural equality on expressions.
-- Note: This does not compute alpha equivalence.
are_structurally_equal_expr :: Expr -> Expr -> Bool
are_structurally_equal_expr x y = let
    are_bindings_eq (Expr_name x, t) (Expr_name x', t') = x == x' && are_structurally_equal_ctype t t'
    are_list_bindings_eq bs bs' = (length bs == length bs') && all (uncurry are_bindings_eq) (zip bs bs')
  in case (x,y) of
    (E_lit_bool b, E_lit_bool b') -> b == b'
    (E_lit_int n, E_lit_int n') -> n == n'
    (E_var x t, E_var x' t') -> x == x' && (are_structurally_equal_qtype t t')
    (E_abs bs e, E_abs bs' e') ->
      (are_list_bindings_eq bs bs') && (are_structurally_equal_expr e e')
    (E_app e es, E_app e' es') ->
      (are_structurally_equal_expr e e') && (length es == length es') &&
        (all (uncurry are_structurally_equal_expr) (zip es es'))
    (E_if e1 e2 e3, E_if e1' e2' e3') ->
      (are_structurally_equal_expr e1 e1') &&
        (are_structurally_equal_expr e2 e2') &&
          (are_structurally_equal_expr e3 e3')
