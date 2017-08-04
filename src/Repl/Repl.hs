{-|
Module      : Repl.Repl
Description : Implementation of the default Toaster REPL.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Repl.Repl ( repl ) where

import Parser ( Ast(..), replParse )
import Expressions ( QType(..), Expr, ExprName(ExprName) )
import Contexts (
  Ctx(..),
  Binding(..),
  extendVar,
  lookupSignature,
  Binding(BVar) )
import Typing ( synthExpr, checkTopLevel )
import Util.DebugOr (
  DebugOr(..),
  showUnderlying,
  mkSuccess,
  isSuccess,
  fromDebug )
import Evaluator ( evalExpr )
import BuiltIns ( builtinsCtx )
import Data.Char ( isSpace, isAlphaNum )
import Data.List ( stripPrefix )
import Repl.State ( CompilerState(..) )
import Repl.Commands ( executeCommand )
import System.IO ( IO, putStr, putStrLn, hFlush, stdout )

-- | The welcome text for the repl.
welcomeText :: String
welcomeText =
  "Welcome to the BT language. For a list of commands, type `:help`."

-- | Immediately flushes the standard output buffer.
flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

-- | Print a prompt and read the input.
prompt :: String -> IO String
prompt s = flushStr s >> getLine

-- | Prints a definition.
showDef :: (ExprName, QType, Expr) -> String
showDef (ExprName s, t, e) =
  "defined " ++ show s ++ ": " ++ show t ++ " := " ++ show e

-- | Attempt to evaluate a top-level definition.
evalDefinition :: CompilerState -> Ast -> DebugOr (CompilerState, String)
evalDefinition state ast@(ADef _ _ _) = do
  (x, t, e) <- checkTopLevel ctx ast
  v <- evalExpr ctx e
  return (CompilerState $ extendVar x t v ctx, showDef x t v)
  where
    ctx = getCtx state
    showDef x t v = "defined " ++ show x ++ ": " ++ show t ++ " := " ++ show v

-- | Evaluates a top-level expression.
evalExpression :: CompilerState -> Ast -> String
evalExpression state ast = showUnderlying v'
  where
    v' = do
      (e, t) <- synthExpr (getCtx state) ast
      v <- evalExpr (getCtx state) e
      return $ show v ++ " : " ++ show t

-- | Attempt to apply Toaster's evaluation rules to compute a value.
tryEvalCode :: CompilerState -> String -> DebugOr (CompilerState, String)
tryEvalCode state source = do
  ast <- replParse source
  case ast of
    ADef _ _ _ -> evalDefinition state ast
    _          -> return (state, evalExpression state ast)

-- | Apply Toaster's evaluation rules to compute a value or print any
-- encountered error and leave the context as it is.
evalCode :: CompilerState -> String -> (CompilerState, String)
evalCode state code = fromDebug attempt id (\x -> (state, x))
  where
    attempt = tryEvalCode state code

-- | Evaluates the input and prints the state.
evalThenPrint2 :: (CompilerState, String) -> (CompilerState, String)
evalThenPrint2 (state, input) = case input of
  []        -> (state, input)
  ':' : cmd -> (state, executeCommand state cmd)
  _         -> evalCode state input

-- | True iff this string is a quit command.
isQuitCmd :: String -> Bool
isQuitCmd s = case s of
  ":quit" -> True
  _       -> False

-- | A farewell.
putGoodbye :: IO ()
putGoodbye = putStrLn "have a good day!"

-- | The header for a prompt.
header :: String
header = "> "

-- | The main loop of the repl.
loop :: CompilerState -> String -> IO ()
loop state input = let (state', result) = evalThenPrint2 (state, input) in
  if isQuitCmd input
    then putGoodbye
    else do
      putStrLn result       -- Print
      cmd <- prompt header  -- Read
      loop state' cmd       -- Eval/Loop

-- | The initial compiler state.
initCompilerState :: CompilerState
initCompilerState = CompilerState builtinsCtx

-- | An entry point into the default Toaster repl.
repl :: IO ()
repl = do
  putStrLn welcomeText
  cmd <- prompt header
  loop initCompilerState cmd
