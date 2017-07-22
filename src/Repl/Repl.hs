{-# LANGUAGE ViewPatterns #-}

module Repl.Repl where

import Parser ( simple_parse )
import Expressions ( QType(..), Expr )
import Contexts ( Ctx(..) )
import Typing ( synth_expr )
import Util.DebugOr ( DebugOr(..), show_underlying )
import Evaluator ( eval_expr )
import BuiltIns ( builtins_ctx )

import Data.Char ( isSpace )
import Control.Monad.Loops ( whileJust_ )
import Data.List ( dropWhile, dropWhileEnd, stripPrefix )

import System.IO ( putStr, putStrLn, hFlush, stdout )

-- | The welcome text for the repl.
welcome_text :: String
welcome_text =
  "Welcome to the BT language. For a list of commands, type `:help`."

-- | The help text.
help_text :: String
help_text =
  "\nHere is a list of commands.\n\
    \ \n\
    \  :ast e    Prints the parse tree of an expression `e`.\n\
    \  :elab e   Prints the elaborated for of the expression `e`.\n\
    \  :help     Print the command list.\n\
    \  :t e      Prints the type of an expression `e`.\n\
    \  :quit     Exit the terminal.\n"

-- | Unrecognized command message.
unrecognized_command :: String -> String
unrecognized_command cmd = "unrecognized command `" ++ cmd ++ "`; try `:help`"

-- | Immediately flushes the standard output buffer.
flush_str :: String -> IO ()
flush_str s = putStr s >> hFlush stdout

-- | Print a prompt and read the input.
prompt :: String -> IO String
prompt s = flush_str s >> getLine

-- | Code that parses, elaborates, and evaluates the code.
eval :: String -> String
eval s = show_underlying v
  where
    v = do
      ast <- simple_parse s
      (e, t) <- synth_expr builtins_ctx ast
      v <- eval_expr e
      return $ show v ++ " : " ++ show t
    pair x y = (x,y)

-- | Removes the command and trailing spaces.
strip_prefix :: String -> String -> Maybe String
strip_prefix cmd s = case stripPrefix cmd s of
  Just []                     -> Just ""
  Just (c : args) | isSpace c -> Just args
  _                           -> Nothing

-- | Prints a parse tree.
parse_tree :: String -> String
parse_tree s = show_underlying $ simple_parse s

-- | Prints the type of an expression.
-- FIXME: We have demonstrated the need to print expressions in a debug and
--        source code mode.
type_synth :: String -> DebugOr (Expr, QType)
type_synth s = do
  ast <- simple_parse s
  (e, t) <- synth_expr builtins_ctx ast
  return (e, t)

-- | Execution of a REPL command.
execute_command :: String -> IO ()
execute_command (strip_prefix "help" -> Just _) = putStrLn help_text
execute_command (strip_prefix "ast"  -> Just x) = putStrLn $ parse_tree x
execute_command (strip_prefix "t"    -> Just x) = putStrLn $ show_underlying $ fmap snd $ type_synth x
execute_command (strip_prefix "elab" -> Just x) = putStrLn $ show_underlying $ fmap fst $ type_synth x
execute_command cmd = putStrLn $ unrecognized_command cmd

-- | Code to evaluate an expression and print it.
eval_and_print :: String -> IO ()
eval_and_print line = case line of
  []        -> putStrLn ""
  ':' : cmd -> execute_command cmd
  _         -> putStrLn $ eval line

-- | Removes preceding and trailing whitespace.
dropWhiteSpace :: String -> String
dropWhiteSpace s = dropWhileEnd isSpace $ dropWhile isSpace s

-- | Runs the prompt and returns the result unless it is the quit command.
prompt_quit_check :: IO (Maybe String)
prompt_quit_check = do
  res <- dropWhiteSpace <$> prompt "> "
  return $ if res == ":quit" then Nothing else Just res

-- | The REPL.
repl :: IO ()
repl = putStrLn welcome_text >> whileJust_ prompt_quit_check eval_and_print