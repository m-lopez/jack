import Test.HUnit( runTestTT )
import ParserSpec ( parserSpec )
import TypingSpec ( typingSpec )
import EvaluationSpec ( evalSpec )

-- | Run all of the unit tests.
main :: IO ()
main = do
  _ <- runTestTT parserSpec
  _ <- runTestTT typingSpec
  _ <- runTestTT evalSpec
  return ()
