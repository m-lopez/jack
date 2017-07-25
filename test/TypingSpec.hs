module TypingSpec ( typingSpec ) where

import Test.HUnit ( assertBool, Test(TestCase, TestList, TestLabel) )

import Parser ( Ast(..), Ast_name (..) )
import Expressions (
  Expr(..),
  QType(..),
  CType(..),
  Expr_name(..),
  are_structurally_equal_expr )
import Typing ( check_expr )
import BuiltIns ( builtinsCtx )
import Contexts ( Ctx(..), Binding(..) )
import Util.DebugOr ( DebugOr(..) )

import Text.Parsec ( parse )



------ Helper functions --------------------------------------------------------

-- An empty context.
emptyCtx :: Ctx
emptyCtx = Ctx [ ]

-- Creates an AST name.
-- FIXME: Put into common test utils module.
sym :: String -> Ast
sym s = A_name $ Ast_name s

var :: String -> QType -> Expr
var x = E_var $ Expr_name x

-- Applies a binary relation under a `DebugOr`.
applyRelation :: (a -> b -> Bool) -> DebugOr a -> DebugOr b -> Bool
applyRelation r a b = case (debug_rep a, debug_rep b) of
  (Right a', Right b') -> r a' b'
  _                    -> False

-- A type check test.
data CheckTest = CheckTest { ctGetCtx  :: Ctx
                           , ctGetCode :: Ast
                           , ctGetType :: QType
                           , ctGetExpr :: Expr }

mkPassingTypeCheckTest :: CheckTest -> Test
mkPassingTypeCheckTest x = TestCase (assertBool name cond)
  where
    name = show ctx ++ " |- " ++ show p ++ ": " ++ show t ++ " => " ++ show e
    cond = applyRelation are_structurally_equal_expr res exp
    res  = DebugOr $ Right e
    exp  = fst <$> check_expr ctx p t
    ctx  = ctGetCtx x
    p    = ctGetCode x
    t    = ctGetType x
    e    = ctGetExpr x

mkPassingTypeCheckTests :: [CheckTest] -> Test
mkPassingTypeCheckTests = TestList . map mkPassingTypeCheckTest



------ Tests -------------------------------------------------------------------

-- Tests on expressions that don't need contexts.
simpleTests :: [CheckTest]
simpleTests = [
  CheckTest
    emptyCtx
    (A_lit_bool True)
    (Unquantified CT_bool)
    (E_lit_bool True),
  CheckTest
    emptyCtx
    (A_lit_int 23)
    (Unquantified CT_int)
    (E_lit_int 23),
  CheckTest
    emptyCtx
    (A_if (A_lit_bool True) (A_lit_int 1) (A_lit_int 2))
    (Unquantified CT_int)
    (E_if (E_lit_bool True) (E_lit_int 1) (E_lit_int 2)) ]

-- Tests on closed expressions.
-- FIXME: Need to support lambdas as a callable expression.
closedTests :: [CheckTest]
closedTests = [ ]
  -- CheckTest
  --   emptyCtx
  --   (A_app (A_abs [ (Ast_name "n", A_type_int) ] (A_lit_int 2)) [ A_lit_int 3 ])
  --   (Unquantified CT_int)
  --   (E_app (E_abs [ (Expr_name "n", CT_int) ] (E_lit_int 2)) [ E_lit_int 3 ]),
  -- CheckTest
  --   emptyCtx
  --   (A_app (A_abs [ (Ast_name "n", A_type_int) ] (sym "n")) [ A_lit_int 3 ])
  --   (Unquantified CT_int)
  --   (E_app (E_abs [ (Expr_name "n", CT_int) ] (var "n" $ Unquantified CT_int)) [ E_lit_int 3 ]) ]

-- Tests on expressions with free variables.
simpleContextTests :: [CheckTest]
simpleContextTests = [
  CheckTest
    (Ctx [ B_var (Expr_name "n") (Unquantified CT_int) ])
    (sym "n")
    (Unquantified CT_int)
    (var "n" $ Unquantified CT_int),
  CheckTest
    (Ctx [ B_var (Expr_name "n") (Unquantified CT_int), B_var (Expr_name "m") (Unquantified CT_int) ])
    (sym "n")
    (Unquantified CT_int)
    (var "n" $ Unquantified CT_int),
  CheckTest
    (Ctx [ B_var (Expr_name "m") (Unquantified CT_int), B_var (Expr_name "n") (Unquantified CT_int) ])
    (sym "n")
    (Unquantified CT_int)
    (var "n" $ Unquantified CT_int) ]

-- Tests that rely on overload resolution.
overloadTests :: [CheckTest]
overloadTests = [
  CheckTest
    (Ctx [ B_var (Expr_name "f") (Unquantified $ CT_arrow [CT_int] CT_int), B_var (Expr_name "f") (Unquantified $ CT_arrow [CT_bool] CT_bool) ])
    (A_app (sym "f") [ A_lit_int 2 ])
    (Unquantified CT_int)
    (E_app (var "f" $ Unquantified $ CT_arrow [CT_int] CT_int) [ E_lit_int 2 ]),
  CheckTest
    (Ctx [ B_var (Expr_name "f") (Unquantified $ CT_arrow [CT_int] CT_int), B_var (Expr_name "f") (Unquantified $ CT_arrow [CT_bool] CT_bool) ])
    (A_app (sym "f") [ A_lit_bool True ])
    (Unquantified CT_bool)
    (E_app (var "f" $ Unquantified $ CT_arrow [CT_bool] CT_bool) [ E_lit_bool True ]) ]

builtinCtxTests :: [CheckTest]
builtinCtxTests = [
  CheckTest
    builtinsCtx
    (A_app (A_name (Ast_name "+")) [A_lit_int 2,A_lit_int 2])
    (Unquantified CT_int)
    (E_app (var "+" $ Unquantified $ CT_arrow [CT_int, CT_int] CT_int) [ E_lit_int 2, E_lit_int 2 ]),
  CheckTest
    builtinsCtx
    (A_app (A_name (Ast_name "rem")) [A_lit_int 2,A_lit_int 2])
    (Unquantified CT_int)
    (E_app (var "rem" $ Unquantified $ CT_arrow [CT_int, CT_int] CT_int) [ E_lit_int 2, E_lit_int 2 ]),
  CheckTest
    builtinsCtx
    (A_app (A_name (Ast_name "=")) [A_lit_int 2,A_lit_int 2])
    (Unquantified CT_bool)
    (E_app (var "=" $ Unquantified $ CT_arrow [CT_int, CT_int] CT_bool) [ E_lit_int 2, E_lit_int 2 ]) ]

-- | Assemble each test suite into a named collection.
typingSpec :: Test
typingSpec = TestLabel "Typing tests" $ TestList [
  TestLabel "simple type check tests" $              mkPassingTypeCheckTests simpleTests,
  TestLabel "closed expression type check tests" $   mkPassingTypeCheckTests closedTests,
  TestLabel "simple context type check tests" $      mkPassingTypeCheckTests simpleContextTests,
  TestLabel "overload resolution type check tests" $ mkPassingTypeCheckTests overloadTests,
  TestLabel "builtin contexts tests" $               mkPassingTypeCheckTests builtinCtxTests ]
