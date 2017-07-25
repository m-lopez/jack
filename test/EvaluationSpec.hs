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
var x = E_var (ExprName x)

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
    (E_lit_int 2)
    (E_lit_int 2),
  EvalEqTest
    (E_lit_bool True)
    (E_lit_bool True),
  EvalEqTest
    (E_abs [] $ E_lit_bool True)
    (E_abs [] $ E_lit_bool True)
  ]

mkBinaryOpType :: CType -> QType
mkBinaryOpType t = Unquantified $ CT_arrow [ t, t ] t

mkBinaryPredType :: CType -> QType
mkBinaryPredType t = Unquantified $ CT_arrow [ t, t ] CT_bool

deltaTests :: [ EvalEqTest ]
deltaTests = [
  EvalEqTest
    (E_app (var "+" $ mkBinaryOpType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_int 5),
  EvalEqTest
    (E_app (var "-" $ mkBinaryOpType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_int (-1)),
  EvalEqTest
    (E_app (var "*" $ mkBinaryOpType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_int 6),
  EvalEqTest
    (E_app (var "/" $ mkBinaryOpType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_int 0),
  EvalEqTest
    (E_app (var "rem" $ mkBinaryOpType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_int 2),
  EvalEqTest
    (E_app (var "=" $ mkBinaryPredType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_bool False),
  EvalEqTest
    (E_app (var "<>" $ mkBinaryPredType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_bool True),
  EvalEqTest
    (E_app (var "<" $ mkBinaryPredType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_bool True),
  EvalEqTest
    (E_app (var "<=" $ mkBinaryPredType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_bool True),
  EvalEqTest
    (E_app (var ">" $ mkBinaryPredType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_bool False),
  EvalEqTest
    (E_app (var ">=" $ mkBinaryPredType CT_int) [ E_lit_int 2, E_lit_int 3 ])
    (E_lit_bool False) ]

-- | Assemble each test suite into a named collection.
evalSpec :: Test
evalSpec = TestLabel "Evaluation Tests" $ TestList [
  TestLabel "value evaluations"    $ mkPassingEvalTests simpleTests,
  TestLabel "test delta functions" $ mkPassingEvalTests deltaTests ]
