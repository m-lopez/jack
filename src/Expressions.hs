-- This module defines the expression languages and fundamental expression
-- algorithms.

module Expressions(
  Expr(..),
  QType(..),
  CType(..),
  ExprName(..),
  TypeName(..),
  substExpr,
  substExprs,
  areStructurallyEqualExpr,
  areStructurallyEqualCType,
  areStructurallyEqualQType ) where

import Util.DebugOr ( DebugOr )
import Data.List ( intercalate )

-- | Symbols.
newtype ExprName = ExprName String deriving (Show, Eq)
newtype TypeName = TypeName String deriving (Show, Eq)

-- | Expression syntax for type checking.
data Expr =
    ELitBool    Bool
  | ELitInt     Integer
  | EVar        ExprName QType
  | EAbs        [(ExprName, CType)] Expr
  | EApp        Expr [Expr]
  | EIf         Expr Expr Expr
  | EUnBuiltin  (Expr -> DebugOr Expr)
  | EBinBuiltin ((Expr, Expr) -> DebugOr Expr)

-- | A printer instance for expressions.
instance Show Expr where
  show e = case e of
    ELitBool b -> show b
    ELitInt n -> show n
    EVar (ExprName x) t -> x ++ "_{" ++ show t ++ "}"
    EAbs bs e1 -> "\\(" ++ intercalate ", " (map show bs) ++ ") -> " ++ show e1
    EApp e1 es -> "(" ++ show e1 ++ ")" ++ "(" ++ intercalate ", " (map show es) ++ ")"
    EIf e1 e2 e3 -> "if " ++ show e1 ++ " then " ++ " else " ++ show e3
    EUnBuiltin _ -> "<built-in>"
    EBinBuiltin _ -> "<built-in>"

-- | Proposition syntax
data Prop = PTrue deriving (Show)

-- | Quantified type level syntax.
data QType =
    Quantified   [TypeName] Prop CType
  | Unquantified CType
  deriving (Show)

-- | Ground type syntax.
data CType =
    CTBool
  | CTInt
  | CTArrow [CType] CType
  | CTVar   TypeName
  deriving (Show)

-- | Variable level substitution of variables.
substExpr :: ExprName -> QType -> Expr -> Expr -> Expr
substExpr x t v e =
  let
    subst_on = substExpr x t v  -- subst on, wayne! subst on, garth!
  in case e of
    ELitBool _      -> e
    ELitInt  _      -> e
    EVar y t'        -> if (x == y) && areStructurallyEqualQType t t' then v else e
    EAbs params body -> if any is_x_t params
                           then e
                           else EAbs params $ subst_on body
      where
        is_x_t (y,t') = (x == y) && (areStructurallyEqualQType t $ Unquantified t')
    EApp e1 args     -> EApp (subst_on e1) (map subst_on args)
    EIf c e1 e2      -> EIf (subst_on c) (subst_on e1) (subst_on e2)

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
  (CTBool, CTBool)         -> True
  (CTInt,  CTInt)          -> True
  (CTArrow src_t1 tgt_t1, CTArrow src_t2 tgt_t2) ->
    let
      same_lengths = length src_t1 == length src_t2
      sources_eq   = all (uncurry areStructurallyEqualCType) (zip src_t1 src_t2)
      targets_eq   = areStructurallyEqualCType tgt_t1 tgt_t2
    in same_lengths && sources_eq && targets_eq
  (CTVar (TypeName x1), CTVar (TypeName x2)) -> x1 == x2
  _ -> False

-- Structural equality on expressions.
-- Note: This does not compute alpha equivalence.
areStructurallyEqualExpr :: Expr -> Expr -> Bool
areStructurallyEqualExpr x y = let
    are_bindings_eq (ExprName z, t) (ExprName z', t') = z == z' && areStructurallyEqualCType t t'
    are_list_bindings_eq bs bs' = (length bs == length bs') && all (uncurry are_bindings_eq) (zip bs bs')
  in case (x,y) of
    (ELitBool b, ELitBool b') -> b == b'
    (ELitInt n, ELitInt n') -> n == n'
    (EVar z t, EVar z' t') -> z == z' && areStructurallyEqualQType t t'
    (EAbs bs e, EAbs bs' e') ->
      are_list_bindings_eq bs bs' && areStructurallyEqualExpr e e'
    (EApp e es, EApp e' es') ->
      areStructurallyEqualExpr e e' && (length es == length es') &&
        all (uncurry areStructurallyEqualExpr) (zip es es')
    (EIf e1 e2 e3, EIf e1' e2' e3') ->
      areStructurallyEqualExpr e1 e1' &&
        areStructurallyEqualExpr e2 e2' &&
          areStructurallyEqualExpr e3 e3'
    _ -> False
