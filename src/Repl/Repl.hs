module Repl.Repl ( repl ) where

import Parser ( Ast(..), replParse )
import Expressions ( QType(..), Expr, ExprName(ExprName) )
import Contexts (
  Ctx(..),
  Binding(..),
  extendVar,
  lookupSignature )
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
import Data.Maybe ( fromMaybe )
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
parseTree s = showUnderlying $ replParse s

-- | Prints the type of an expression.
-- FIXME: We have demonstrated the need to print expressions in a debug and
--        source code mode.
typeSynth :: String -> DebugOr (Expr, QType)
typeSynth s = do
  ast <- replParse s
  (e, t) <- synthExpr builtinsCtx ast
  return (e, t)

-- | Removes preceding and trailing whitespace.
dropWhiteSpace :: String -> String
dropWhiteSpace s = dropWhileEnd isSpace $ dropWhile isSpace s

-- | Compiler state.
newtype CompilerState = CompilerState { getCtx :: Ctx }

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

-- | Print the current bindings.
-- bindingsCmd :: String -> String
-- bindingsCmd arg = if arg == ""
--   then showContext
--   else "the command `bindings` does not take any arguments"

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
  Command "ast" astCmd "<e>"
    "Prints the parse tree of an expression `e`.",
  Command "elab" elabCmd "<e>"
    "Prints the elaborated for of the expression `e`.",
  Command "help" helpCmd ""
    "Print the command list.",
  -- Command "bindigs" bindingsCmd ""
  --   "Prints all bindings in the current context",
  Command "t" tCmd "<e>"
    "Prints the type of an expression `e`." ]

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
