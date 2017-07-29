module ParserSpec ( parserSpec ) where

import Test.HUnit ( assertEqual, Test(TestCase, TestList, TestLabel) )
import Parser ( simpleParse, Ast(..), AstName (..) )
import Util.DebugOr ( debugRep )



------ Helper functions --------------------------------------------------------

-- Creates an AST name.
sym :: String -> Ast
sym = AName . AstName

-- Run the parser
tryParse :: String -> Either String Ast
tryParse s = case debugRep $ simpleParse s of
  Left x  -> Left $ show x
  Right x -> Right x

-- `SyntaxTest` represents a test where `code` must parse to `expected`.
data SyntaxTest = SyntaxTest { codeSnippet    :: String
                             , expectedResult :: Ast }

mkSyntaxTest :: SyntaxTest -> Test
mkSyntaxTest t = TestCase (assertEqual code expected actual)
  where
    code     = codeSnippet t
    expected = Right $ expectedResult t
    actual   = tryParse $ codeSnippet t

mkSyntaxTests :: [SyntaxTest] -> Test
mkSyntaxTests = TestList . map mkSyntaxTest



------ Tests -------------------------------------------------------------------

-- Simple expressions
simpleExprTests :: [SyntaxTest]
simpleExprTests = [
  SyntaxTest "23"      (ALitInt 23),
  SyntaxTest "true"    (ALitBool True),
  SyntaxTest "false"   (ALitBool False),
  SyntaxTest "(1)"     (ALitInt 1),
  SyntaxTest "((1))"   (ALitInt 1),
  SyntaxTest "(((1)))" (ALitInt 1) ]

-- Binary operations tests.
binOpTests :: [SyntaxTest]
binOpTests = [
  SyntaxTest
    "n + m"
    (AApp (sym "+") [ sym "n", sym "m" ]),
  SyntaxTest
    "n - m"
    (AApp (sym "-") [ sym "n", sym "m" ]),
  SyntaxTest
    "n - -m"
    (AApp (sym "-") [ sym "n", AApp (sym "-") [ sym "m" ]]),
  SyntaxTest
    "n rem 2 = k"
    (AApp (sym "=") [ AApp (sym "rem") [ sym "n", ALitInt 2 ], sym "k" ]),
  SyntaxTest
    "n rem 2"
    (AApp (sym "rem") [ sym "n", ALitInt 2 ]) ]

-- Lambda tests.
lambdaTests :: [SyntaxTest]
lambdaTests = [
  SyntaxTest
    "\\(n: Int) n"
    (AAbs [(AstName "n", ATypeInt)] $ sym "n"),
  SyntaxTest
    "\\(n: Int, m: Int) n + m"
    (AAbs [(AstName "n", ATypeInt), (AstName "m", ATypeInt)] $ AApp (sym "+") [ sym "n", sym "m" ]),
  SyntaxTest
    "\\(n:Int)\\(m:Int) n + m"
    (AAbs [(AstName "n", ATypeInt)] $ AAbs [(AstName "m", ATypeInt)] $ AApp (sym "+") [ sym "n", sym "m" ])
  ]

-- Conditional tests.
conditionalTests :: [SyntaxTest]
conditionalTests = [
  SyntaxTest
    "if false then 1 else 2"
    (AIf (ALitBool False) (ALitInt 1) (ALitInt 2)),
  SyntaxTest
    "if b then \\(n:Int) n - 1 else \\(m:Int) m + 1"
    (AIf (sym "b") (AAbs [(AstName "n", ATypeInt)] (AApp (sym "-") [ sym "n", ALitInt 1 ])) (AAbs [(AstName "m", ATypeInt)] (AApp (sym "+") [ (sym "m"), (ALitInt 1) ])))
  ]

-- Application tests.
appTests :: [SyntaxTest]
appTests = [
  SyntaxTest
    "f x"
    (AApp (sym "f") [ sym "x" ]),
  SyntaxTest
    "f -x" -- If we want to treat `-` as unary, then we would get `n-m` -> `n(-m)`.
    (AApp (sym "-") [ sym "f", sym "x" ]),
  SyntaxTest
    "f (-x)"
    (AApp (sym "f") [ AApp (sym "-") [ sym "x" ] ]),
  SyntaxTest
    "f g x"
    (AApp (sym "f") [ AApp (sym "g") [ sym "x" ] ]),
  SyntaxTest
    "f x + 1"
    (AApp (sym "+") [ AApp (sym "f") [ sym "x"], ALitInt 1 ]),
  SyntaxTest
    "size xs = size ys and same_elts (xs, ys)"
    (AApp (sym "and") [ AApp (sym "=") [ AApp (sym "size") [ sym "xs" ], AApp (sym "size") [ sym "ys" ] ], AApp (sym "same_elts") [ sym "xs", sym "ys" ] ] ) ]

-- | Assemble each test suite into a named collection.
parserSpec :: Test
parserSpec = TestLabel "Parsing tests" $ TestList [
  TestLabel "simple expressions" $            mkSyntaxTests simpleExprTests,
  TestLabel "binary operation syntax tests" $ mkSyntaxTests binOpTests,
  TestLabel "lambda syntax tests" $           mkSyntaxTests lambdaTests,
  TestLabel "conditional syntax tests" $      mkSyntaxTests conditionalTests,
  TestLabel "application syntax tests" $      mkSyntaxTests appTests ]
