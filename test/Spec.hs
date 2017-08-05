import Test.HUnit( runTestTT )
import ParserSpec ( parserSpec )
import ElaborationSpec ( elaborationSpec )
import EvaluatorSpec ( evalSpec )

-- | Run all of the unit tests.
main :: IO ()
main = do
  _ <- runTestTT parserSpec
  _ <- runTestTT elaborationSpec
  _ <- runTestTT evalSpec
  return ()
