import Test.HUnit( Test(TestList, TestLabel), runTestTT )
import ParserSpec ( parserSpec )
import TypingSpec ( typingSpec )
import EvaluationSpec ( evalSpec )

main :: IO ()
main = do
  runTestTT parserSpec
  runTestTT typingSpec
  runTestTT evalSpec
  return ()
