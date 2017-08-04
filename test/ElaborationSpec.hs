module ElaborationSpec ( elaborationSpec ) where

import Test.HUnit ( assertBool, Test(TestCase, TestList, TestLabel) )

import Parser ( Ast(..), AstName (..) )
import Expressions (
  Expr(..),
  QType(..),
  CType(..),
  ExprName(..),
  areStructurallyEqualExpr )
import Elaboration ( checkExpr )
import BuiltIns ( builtinsCtx )
import Context ( Ctx(..), Binding(..) )
import Util.DebugOr ( DebugOr(..) )



------ Helper functions --------------------------------------------------------

-- An empty context.
emptyCtx :: Ctx
emptyCtx = Ctx [ ]

-- Creates an AST name.
-- FIXME: Put into common test utils module.
sym :: String -> Ast
sym = AName . AstName

var :: String -> QType -> Expr
var = EVar . ExprName

-- Applies a binary relation under a `DebugOr`.
applyRelation :: (a -> b -> Bool) -> DebugOr a -> DebugOr b -> Bool
applyRelation r a b = case (debugRep a, debugRep b) of
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
    name = " |- " ++ show p ++ ": " ++ show t ++ " => " ++ show e
    cond = applyRelation areStructurallyEqualExpr res expd
    res  = DebugOr $ Right e
    expd = fst <$> checkExpr ctx p t
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
    (ALitBool True)
    (Unquantified CTBool)
    (ELitBool True),
  CheckTest
    emptyCtx
    (ALitInt 23)
    (Unquantified CTInt)
    (ELitInt 23),
  CheckTest
    emptyCtx
    (AIf (ALitBool True) (ALitInt 1) (ALitInt 2))
    (Unquantified CTInt)
    (EIf (ELitBool True) (ELitInt 1) (ELitInt 2)) ]

-- Tests on closed expressions.
-- FIXME: Need to support lambdas as a callable expression.
closedTests :: [CheckTest]
closedTests = [ ]
  -- CheckTest
  --   emptyCtx
  --   (AApp (AAbs [ (AstName "n", ATypeInt) ] (ALitInt 2)) [ ALitInt 3 ])
  --   (Unquantified CTInt)
  --   (EApp (EAbs [ (ExprName "n", CTInt) ] (ELitInt 2)) [ ELitInt 3 ]),
  -- CheckTest
  --   emptyCtx
  --   (AApp (AAbs [ (AstName "n", ATypeInt) ] (sym "n")) [ ALitInt 3 ])
  --   (Unquantified CTInt)
  --   (EApp (EAbs [ (ExprName "n", CTInt) ] (var "n" $ Unquantified CTInt)) [ ELitInt 3 ]) ]

-- Tests on expressions with free variables.
simpleContextTests :: [CheckTest]
simpleContextTests = [
  CheckTest
    (Ctx [ BVar (ExprName "n") (Unquantified CTInt) Nothing ])
    (sym "n")
    (Unquantified CTInt)
    (var "n" $ Unquantified CTInt),
  CheckTest
    (Ctx [ BVar (ExprName "n") (Unquantified CTInt) Nothing, BVar (ExprName "m") (Unquantified CTInt) Nothing ])
    (sym "n")
    (Unquantified CTInt)
    (var "n" $ Unquantified CTInt),
  CheckTest
    (Ctx [ BVar (ExprName "m") (Unquantified CTInt) Nothing, BVar (ExprName "n") (Unquantified CTInt) Nothing ])
    (sym "n")
    (Unquantified CTInt)
    (var "n" $ Unquantified CTInt) ]

-- Tests that rely on overload resolution.
overloadTests :: [CheckTest]
overloadTests = [
  CheckTest
    (Ctx [ BVar (ExprName "f") (Unquantified $ CTArrow [CTInt] CTInt) Nothing, BVar (ExprName "f") (Unquantified $ CTArrow [CTBool] CTBool) Nothing ])
    (AApp (sym "f") [ ALitInt 2 ])
    (Unquantified CTInt)
    (EApp (var "f" $ Unquantified $ CTArrow [CTInt] CTInt) [ ELitInt 2 ]),
  CheckTest
    (Ctx [ BVar (ExprName "f") (Unquantified $ CTArrow [CTInt] CTInt) Nothing, BVar (ExprName "f") (Unquantified $ CTArrow [CTBool] CTBool) Nothing ])
    (AApp (sym "f") [ ALitBool True ])
    (Unquantified CTBool)
    (EApp (var "f" $ Unquantified $ CTArrow [CTBool] CTBool) [ ELitBool True ]) ]

builtinCtxTests :: [CheckTest]
builtinCtxTests = [
  CheckTest
    builtinsCtx
    (AApp (AName (AstName "+")) [ALitInt 2,ALitInt 2])
    (Unquantified CTInt)
    (EApp (var "+" $ Unquantified $ CTArrow [CTInt, CTInt] CTInt) [ ELitInt 2, ELitInt 2 ]),
  CheckTest
    builtinsCtx
    (AApp (AName (AstName "rem")) [ALitInt 2,ALitInt 2])
    (Unquantified CTInt)
    (EApp (var "rem" $ Unquantified $ CTArrow [CTInt, CTInt] CTInt) [ ELitInt 2, ELitInt 2 ]),
  CheckTest
    builtinsCtx
    (AApp (AName (AstName "=")) [ALitInt 2,ALitInt 2])
    (Unquantified CTBool)
    (EApp (var "=" $ Unquantified $ CTArrow [CTInt, CTInt] CTBool) [ ELitInt 2, ELitInt 2 ]) ]

-- | Assemble each test suite into a named collection.
elaborationSpec :: Test
elaborationSpec = TestLabel "Typing tests" $ TestList [
  TestLabel "simple type check tests" $              mkPassingTypeCheckTests simpleTests,
  TestLabel "closed expression type check tests" $   mkPassingTypeCheckTests closedTests,
  TestLabel "simple context type check tests" $      mkPassingTypeCheckTests simpleContextTests,
  TestLabel "overload resolution type check tests" $ mkPassingTypeCheckTests overloadTests,
  TestLabel "builtin contexts tests" $               mkPassingTypeCheckTests builtinCtxTests ]
