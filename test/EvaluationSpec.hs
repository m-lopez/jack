module EvaluationSpec ( evalSpec ) where

import Test.HUnit ( assertBool, Test(TestCase, TestList, TestLabel) )
import Expressions (
  Expr(..),
  CType(..),
  QType(..),
  ExprName(..),
  areStructurallyEqualExpr )
import Evaluator ( evalExpr )
import Util.DebugOr ( DebugOr(debugRep), mkSuccess )



------ Helper functions --------------------------------------------------------

-- FIXME: Move this shorthand into a test utils.
var :: String -> QType -> Expr
var x = EVar (ExprName x)

-- Applies a binary relation under a `DebugOr`.
-- FIXME: Add this to a common collection of test utilities.
applyRelation :: (a -> b -> Bool) -> DebugOr a -> DebugOr b -> Bool
applyRelation r a b = case (debugRep a, debugRep b) of
  (Right a', Right b') -> r a' b'
  _                    -> False

data EvalEqTest = EvalEqTest { getUneval :: Expr, getExpectedValue :: Expr }

mkPassingEvalTest :: EvalEqTest -> Test
mkPassingEvalTest x = TestCase $ assertBool name cond
  where
    e    = getUneval x
    v    = evalExpr e
    expe = getExpectedValue x
    name = show e ++ " --> " ++ show expe
    cond = applyRelation areStructurallyEqualExpr (mkSuccess expe) v

mkPassingEvalTests :: [ EvalEqTest ] -> Test
mkPassingEvalTests = TestList . map mkPassingEvalTest



------ Tests -------------------------------------------------------------------

simpleTests :: [ EvalEqTest ]
simpleTests = [
  EvalEqTest
    (ELitInt 2)
    (ELitInt 2),
  EvalEqTest
    (ELitBool True)
    (ELitBool True),
  EvalEqTest
    (EAbs [] $ ELitBool True)
    (EAbs [] $ ELitBool True)
  ]

mkBinaryOpType :: CType -> QType
mkBinaryOpType t = Unquantified $ CTArrow [ t, t ] t

mkBinaryPredType :: CType -> QType
mkBinaryPredType t = Unquantified $ CTArrow [ t, t ] CTBool

deltaTests :: [ EvalEqTest ]
deltaTests = [
  EvalEqTest
    (EApp (var "+" $ mkBinaryOpType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitInt 5),
  EvalEqTest
    (EApp (var "-" $ mkBinaryOpType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitInt (-1)),
  EvalEqTest
    (EApp (var "*" $ mkBinaryOpType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitInt 6),
  EvalEqTest
    (EApp (var "/" $ mkBinaryOpType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitInt 0),
  EvalEqTest
    (EApp (var "rem" $ mkBinaryOpType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitInt 2),
  EvalEqTest
    (EApp (var "=" $ mkBinaryPredType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitBool False),
  EvalEqTest
    (EApp (var "<>" $ mkBinaryPredType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitBool True),
  EvalEqTest
    (EApp (var "<" $ mkBinaryPredType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitBool True),
  EvalEqTest
    (EApp (var "<=" $ mkBinaryPredType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitBool True),
  EvalEqTest
    (EApp (var ">" $ mkBinaryPredType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitBool False),
  EvalEqTest
    (EApp (var ">=" $ mkBinaryPredType CTInt) [ ELitInt 2, ELitInt 3 ])
    (ELitBool False) ]

-- | Assemble each test suite into a named collection.
evalSpec :: Test
evalSpec = TestLabel "Evaluation Tests" $ TestList [
  TestLabel "value evaluations"    $ mkPassingEvalTests simpleTests,
  TestLabel "test delta functions" $ mkPassingEvalTests deltaTests ]
