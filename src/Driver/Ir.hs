{-|
Module      : Driver.Ir
Description : Facilities for writing IR of different stages to a file.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Driver.Ir ( writeIr ) where

import qualified Driver.Options as Options ( Stage(..) )
import Ast.Parser ( parseModule )
import Elaboration ( elabModule )
import Util.DebugOr ( DebugOr, fromDebugOr )
import System.Directory ( doesFileExist )
import Builtins ( builtinsCtx )
import Expressions (TlExpr)
import Context ( Ctx )



elabSource :: String -> DebugOr [TlExpr]
elabSource src = fst <$> (parseModule src >>= elabModule builtinsCtx)

{-
toLlvmIrFromSource :: String -> DebugOr (IO String)
toLlvmIrFromSource src = elabSource src >>= toLlvmIr
-}

writeOrErr :: DebugOr String -> FilePath -> IO ()
writeOrErr sDbg o = fromDebugOr sDbg writeOut showErr
  where
    showErr x = putStrLn $ "cannot write IR: " ++ x
    writeOut = writeFile o

writeEffectfulOrErr :: DebugOr (IO String) -> FilePath -> IO ()
writeEffectfulOrErr effDbgS o = fromDebugOr effDbgS writeEff showErr
  where
    showErr x = putStrLn $ "cannot write IR: " ++ x
    writeEff x = x >>= writeFile o

execStage :: Options.Stage -> String -> FilePath -> IO ()
execStage s src = case s of
  Options.Ast  -> writeOrErr (show <$> parseModule src)
  Options.Elab -> writeOrErr (show <$> elabSource src)
  Options.Llvm -> \x -> putStrLn "error: llvm not yet supported" -- writeEffectfulOrErr (toLlvmIrFromSource src)


writeIr :: Options.Stage -> FilePath -> FilePath -> IO ()
writeIr s i o = do
  exists <- doesFileExist i
  if exists
    then do
      contents <- readFile i
      execStage s contents o
    else putStrLn $ "file " ++ i ++ "does not exist"
