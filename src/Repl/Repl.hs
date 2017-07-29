module Repl.Repl ( repl ) where

import Parser ( simpleParse )
import Expressions ( QType(..), Expr )
import Contexts ( Ctx(..) )
import Typing ( synthExpr )
import Util.DebugOr ( DebugOr(..), showUnderlying )
import Evaluator ( evalExpr )
import BuiltIns ( builtinsCtx )

import Data.Char ( isSpace )
import Data.Maybe ( fromMaybe )
import Control.Monad.Loops ( whileJust_ )
import Data.List ( dropWhile, dropWhileEnd, stripPrefix )

import System.IO ( IO, putStr, putStrLn, hFlush, stdout )

-- | The welcome text for the repl.
welcomeText :: String
welcomeText =
  "Welcome to the BT language. For a list of commands, type `:help`."

-- | Unrecognized command message.
unrecognizedCommand :: String -> String
unrecognizedCommand cmd = "unrecognized command `" ++ cmd ++ "`; try `:help`"

-- | Immediately flushes the standard output buffer.
flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

-- | Print a prompt and read the input.
prompt :: String -> IO String
prompt s = flushStr s >> getLine

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

-- | Removes preceding and trailing whitespace.
dropWhiteSpace :: String -> String
dropWhiteSpace s = dropWhileEnd isSpace $ dropWhile isSpace s

data Store = Store

-- | Compiler state.
data CompilerState = CompilerState {
  getCtx   :: Ctx,
  getStore :: Store }

-- | Prints all of the commands.
helpCmd :: String -> String
helpCmd arg = if arg == ""
  then helpText
  else "the command `help` does not take any arguments"

-- | Elaborates an expression.
elabCmd :: String -> String
elabCmd arg = showUnderlying $ fst <$> typeSynth arg

-- | Print the parse tree of code.
astCmd :: String -> String
astCmd = parseTree

-- | Print the type of an expression.
tCmd :: String -> String
tCmd arg = showUnderlying $ snd <$> typeSynth arg

-- | A mock command for exiting the terminal used for the description of
-- commands.
mockQuitCmd :: Command
mockQuitCmd = Command "quit" id "" "Exit the terminal."

-- | The help text.
-- FIXME: Break this up and put it in another file!
helpText :: String
helpText = header ++ "\n" ++ concat descLines
  where
    header = "Here is a list of commands."
    allCommands = commands ++ [ mockQuitCmd ]
    indent = "  "
    argColLength = foldl max 0 $ map (\x -> ((length $ getExampleArgs x) + (length $ getName x))) allCommands
    printCmdLine (Command name _ args desc) = indent ++ ":" ++ name ++ " " ++ args ++ replicate (argColLength - (length args + length name) + 1) ' ' ++ desc ++ "\n"
    descLines = map printCmdLine allCommands

-- | A Toaster REPL command. There is always the implicit.
data Command = Command {
  getName ::        String,
  getProcedure ::   String -> String,
  getExampleArgs :: String,
  getDescription :: String
}

-- | A command is a string and a function that parsers the preceeding arguments.
-- FIXME: Move this to another file.
commands :: [Command]
commands = [
  Command "help" helpCmd ""    "Print the command list.",
  Command "elab" elabCmd "<e>" "Prints the elaborated for of the expression `e`.",
  Command "ast"  astCmd  "<e>" "Prints the parse tree of an expression `e`.",
  Command "t"    tCmd    "<e>" "Prints the type of an expression `e`." ]

-- | Matches the input command `cmd` with one of the commands.
matchCmd :: String -> Maybe String
matchCmd cmd = matchCmdRec cmd commands
  where
    matchCmdRec cmd []     = Nothing
    matchCmdRec cmd (c:cs) = case stripPrefix (getName c) cmd of
      Just arg -> Just $ getProcedure c $ dropWhiteSpace arg
      Nothing  -> matchCmdRec cmd cs

-- | Executes a command.
execCmd :: String -> String
execCmd cmd = fromMaybe (unrecognizedCommand cmd) (matchCmd cmd)

-- | Applies Toaster's evaluation rules to compute a value.
evalCode :: CompilerState -> String -> (CompilerState, String)
evalCode state input = (state, showUnderlying v)
  where
    v = do
      ast <- simpleParse input
      (e, t) <- synthExpr (getCtx state) ast
      v <- evalExpr e
      return $ show v ++ " : " ++ show t
    pair x y = (x,y)

-- | Evaluates the input and prints the state.
evalThenPrint2 :: (CompilerState, String) -> (CompilerState, String)
evalThenPrint2 (state, input) = case input of
  []        -> (state, input)
  ':' : cmd -> (state, execCmd cmd)
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
loop state input = if isQuitCmd input
  then putGoodbye
  else do
    putStrLn result       -- Print
    cmd <- prompt header  -- Read
    loop  state cmd       -- Eval/Loop
  where (state, result) = evalThenPrint2 (state, input)

initCompilerState :: CompilerState
initCompilerState = CompilerState builtinsCtx Store

-- | An entry point into the default Toaster repl.
repl :: IO ()
repl = do
  putStrLn welcomeText
  cmd <- prompt "new >" -- header
  loop initCompilerState cmd
