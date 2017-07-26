{-# LANGUAGE ViewPatterns #-}

module Repl.Repl where

import Parser ( simpleParse )
import Expressions ( QType(..), Expr )
import Contexts ( Ctx(..) )
import Typing ( synthExpr )
import Util.DebugOr ( DebugOr(..), showUnderlying )
import Evaluator ( evalExpr )
import BuiltIns ( builtinsCtx )

import Data.Char ( isSpace )
import Control.Monad.Loops ( whileJust_ )
import Data.List ( dropWhile, dropWhileEnd, stripPrefix )

import System.IO ( putStr, putStrLn, hFlush, stdout )

-- | The welcome text for the repl.
welcomeText :: String
welcomeText =
  "Welcome to the BT language. For a list of commands, type `:help`."

-- | The help text.
helpText :: String
helpText =
  "\nHere is a list of commands.\n\
    \ \n\
    \  :ast e    Prints the parse tree of an expression `e`.\n\
    \  :elab e   Prints the elaborated for of the expression `e`.\n\
    \  :help     Print the command list.\n\
    \  :t e      Prints the type of an expression `e`.\n\
    \  :quit     Exit the terminal.\n"

-- | Unrecognized command message.
unrecognizedCommand :: String -> String
unrecognizedCommand cmd = "unrecognized command `" ++ cmd ++ "`; try `:help`"

-- | Immediately flushes the standard output buffer.
flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

-- | Print a prompt and read the input.
prompt :: String -> IO String
prompt s = flushStr s >> getLine

-- | Code that parses, elaborates, and evaluates the code.
eval :: String -> String
eval s = showUnderlying v
  where
    v = do
      ast <- simpleParse s
      (e, t) <- synthExpr builtinsCtx ast
      v <- evalExpr e
      return $ show v ++ " : " ++ show t
    pair x y = (x,y)

-- | Removes the command and trailing spaces.
strip_prefix :: String -> String -> Maybe String
strip_prefix cmd s = case stripPrefix cmd s of
  Just []                     -> Just ""
  Just (c : args) | isSpace c -> Just args
  _                           -> Nothing

-- | Prints a parse tree.
parseTree :: String -> String
parseTree s = showUnderlying $ simpleParse s

-- | Prints the type of an expression.
-- FIXME: We have demonstrated the need to print expressions in a debug and
--        source code mode.
typeSynth :: String -> DebugOr (Expr, QType)
typeSynth s = do
  ast <- simpleParse s
  (e, t) <- synthExpr builtinsCtx ast
  return (e, t)

-- | Execution of a REPL command.
executeCommand :: String -> IO ()
executeCommand (strip_prefix "help" -> Just _) = putStrLn helpText
executeCommand (strip_prefix "ast"  -> Just x) = putStrLn $ parseTree x
executeCommand (strip_prefix "t"    -> Just x) = putStrLn $ showUnderlying $ snd <$> typeSynth x
executeCommand (strip_prefix "elab" -> Just x) = putStrLn $ showUnderlying $ fst <$> typeSynth x
executeCommand cmd = putStrLn $ unrecognizedCommand cmd

-- | Code to evaluate an expression and print it.
evalThenPrint :: String -> IO ()
evalThenPrint line = case line of
  []        -> putStrLn ""
  ':' : cmd -> executeCommand cmd
  _         -> putStrLn $ eval line

-- | Removes preceding and trailing whitespace.
dropWhiteSpace :: String -> String
dropWhiteSpace s = dropWhileEnd isSpace $ dropWhile isSpace s

-- | Runs the prompt and returns the result unless it is the quit command.
promptQuitCheck :: IO (Maybe String)
promptQuitCheck = do
  res <- dropWhiteSpace <$> prompt "> "
  return $ if res == ":quit" then Nothing else Just res

-- | The REPL.
repl :: IO ()
repl = putStrLn welcomeText >> whileJust_ promptQuitCheck evalThenPrint
