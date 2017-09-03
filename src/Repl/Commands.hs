{-|
Module      : Repl.Commands
Description : Facilities for commands available in the REPL.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Repl.Commands(
  executeCommand
) where

import Repl.State ( CompilerState(..) )
import Context ( Ctx(..), Binding(BVar) )
import Expressions ( QType, ExprName, Expr, ExprName(ExprName) )
import Util.DebugOr ( DebugOr, showUnderlying )
import Data.List ( dropWhile, dropWhileEnd, stripPrefix )
import Data.Char ( isSpace )
import Ast.Parser ( Ast(..), replParse )
import Elaboration ( synthExpr )
import Data.Maybe ( fromMaybe )



-- | Unrecognized command message.
unrecognizedCommand :: String -> String
unrecognizedCommand cmd = "unrecognized command `" ++ cmd ++ "`; try `:help`"

-- | Removes preceding and trailing whitespace.
dropEnclosingWhiteSpace :: String -> String
dropEnclosingWhiteSpace s = dropWhileEnd isSpace $ dropWhile isSpace s

-- | Prints a parse tree.
parseTree :: String -> String
parseTree s = showUnderlying $ replParse s

-- | Prints the type of an expression.
-- FIXME: We have demonstrated the need to print expressions in a debug and
--        source code mode.
typeSynth :: CompilerState -> String -> DebugOr (Expr, QType)
typeSynth state s = do
  ast <- replParse s
  (e, t) <- synthExpr (getCtx state) ast
  return (e, t)

-- | Prints all of the commands.
helpCmd :: CompilerState -> String -> String
helpCmd _ arg = if arg == ""
  then helpText
  else "the command `help` does not take any arguments"

-- | Elaborates an expression.
elabCmd :: CompilerState -> String -> String
elabCmd state arg = showUnderlying $ fst <$> typeSynth state arg

-- | Print the parse tree of code.
astCmd :: CompilerState -> String -> String
astCmd state = parseTree

-- | Print the type of an expression.
tCmd :: CompilerState -> String -> String
tCmd state arg = showUnderlying $ snd <$> typeSynth state arg

-- | Print the current bindings.
bindingsCmd :: CompilerState -> String -> String
bindingsCmd (CompilerState (Ctx bindings)) arg = if arg == ""
  then showContext bindings
  else "the command `bindings` does not take any arguments"
  where
    showContext ctx' = case ctx' of
      BVar (ExprName x) t _ : ctx'' -> x ++ ": " ++ show t ++ "\n" ++ showContext ctx''
      [] -> ""

-- | A mock command for exiting the terminal used for the description of
-- commands.
mockQuitCmd :: Command
mockQuitCmd = Command "quit" emptyCmd "" "Exit the terminal."
  where
    emptyCmd x y = ""

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
  getProcedure ::   CompilerState -> String -> String,
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
  Command "bindings" bindingsCmd ""
    "Prints all bindings in the current context",
  Command "t" tCmd "<e>"
    "Prints the type of an expression `e`." ]

-- | Matches the input command `cmd` with one of the commands.
matchCmd :: CompilerState -> String -> Maybe String
matchCmd state cmd = matchCmdRec cmd commands
  where
    matchCmdRec :: String -> [Command] -> Maybe String
    matchCmdRec cmd []     = Nothing
    matchCmdRec cmd (c:cs) = case stripPrefix (getName c) cmd of
      Just arg -> Just $ func state justArg
        where
          func = getProcedure c
          justArg = dropEnclosingWhiteSpace arg
      Nothing  -> matchCmdRec cmd cs

-- | Executes a command.
executeCommand :: CompilerState -> String -> String
executeCommand state cmd = fromMaybe (unrecognizedCommand cmd) (matchCmd state cmd)

