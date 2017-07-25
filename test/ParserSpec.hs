module ParserSpec ( parserSpec ) where

import Test.HUnit ( assertEqual, Test(TestCase, TestList, TestLabel) )
import Parser ( simpleParse, Ast(..), AstName (..) )
import Text.Parsec ( parse )
import Util.DebugOr ( debugRep )



------ Helper functions --------------------------------------------------------

-- Creates an AST name.
sym :: String -> Ast
sym = A_name . AstName

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
  SyntaxTest "23"      (A_lit_int 23),
  SyntaxTest "true"    (A_lit_bool True),
  SyntaxTest "false"   (A_lit_bool False),
  SyntaxTest "(1)"     (A_lit_int 1),
  SyntaxTest "((1))"   (A_lit_int 1),
  SyntaxTest "(((1)))" (A_lit_int 1) ]

-- Binary operations tests.
binOpTests :: [SyntaxTest]
binOpTests = [
  SyntaxTest
    "n + m"
    (A_app (sym "+") [ sym "n", sym "m" ]),
  SyntaxTest
    "n - m"
    (A_app (sym "-") [ sym "n", sym "m" ]),
  SyntaxTest
    "n - -m"
    (A_app (sym "-") [ sym "n", A_app (sym "-") [ sym "m" ]]),
  SyntaxTest
    "n rem 2 = k"
    (A_app (sym "=") [ A_app (sym "rem") [ sym "n", A_lit_int 2 ], sym "k" ]),
  SyntaxTest
    "n rem 2"
    (A_app (sym "rem") [ sym "n", A_lit_int 2 ]) ]

-- Lambda tests.
lambdaTests :: [SyntaxTest]
lambdaTests = [
  SyntaxTest
    "\\(n: Int) n"
    (A_abs [(AstName "n", A_type_int)] $ sym "n"),
  SyntaxTest
    "\\(n: Int, m: Int) n + m"
    (A_abs [(AstName "n", A_type_int), (AstName "m", A_type_int)] $ A_app (sym "+") [ sym "n", sym "m" ]),
  SyntaxTest
    "\\(n:Int)\\(m:Int) n + m"
    (A_abs [(AstName "n", A_type_int)] $ A_abs [(AstName "m", A_type_int)] $ A_app (sym "+") [ sym "n", sym "m" ])
  ]

-- Conditional tests.
conditionalTests :: [SyntaxTest]
conditionalTests = [
  SyntaxTest
    "if false then 1 else 2"
    (A_if (A_lit_bool False) (A_lit_int 1) (A_lit_int 2)),
  SyntaxTest
    "if b then \\(n:Int) n - 1 else \\(m:Int) m + 1"
    (A_if (sym "b") (A_abs [(AstName "n", A_type_int)] (A_app (sym "-") [ sym "n", A_lit_int 1 ])) (A_abs [(AstName "m", A_type_int)] (A_app (sym "+") [ (sym "m"), (A_lit_int 1) ])))
  ]

-- Application tests.
appTests :: [SyntaxTest]
appTests = [
  SyntaxTest
    "f x"
    (A_app (sym "f") [ sym "x" ]),
  SyntaxTest
    "f -x" -- If we want to treat `-` as unary, then we would get `n-m` -> `n(-m)`.
    (A_app (sym "-") [ sym "f", sym "x" ]),
  SyntaxTest
    "f (-x)"
    (A_app (sym "f") [ A_app (sym "-") [ sym "x" ] ]),
  SyntaxTest
    "f g x"
    (A_app (sym "f") [ A_app (sym "g") [ sym "x" ] ]),
  SyntaxTest
    "f x + 1"
    (A_app (sym "+") [ A_app (sym "f") [ sym "x"], A_lit_int 1 ]),
  SyntaxTest
    "size xs = size ys and same_elts (xs, ys)"
    (A_app (sym "and") [ A_app (sym "=") [ A_app (sym "size") [ sym "xs" ], A_app (sym "size") [ sym "ys" ] ], A_app (sym "same_elts") [ sym "xs", sym "ys" ] ] ) ]

-- | Assemble each test suite into a named collection.
parserSpec :: Test
parserSpec = TestLabel "Parsing tests" $ TestList [
  TestLabel "simple expressions" $            mkSyntaxTests simpleExprTests,
  TestLabel "binary operation syntax tests" $ mkSyntaxTests binOpTests,
  TestLabel "application syntax tests" $      mkSyntaxTests appTests ]
