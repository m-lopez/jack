{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : Driver.Options
Description : A command-line options processor.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Driver.Options ( Stage(..), Options(..), processOptions ) where

import System.Console.CmdArgs

-- | The stage from which the IR is emitted.
data Stage =
  Ast |
  Elab |
  Llvm
    deriving (Data, Typeable, Show, Eq)

-- | Processed form of arguments.
data Options =
    Repl                        -- Run the repl.
  | Ir { stage :: Stage         -- Write human readable IR of a stage.
       , input :: FilePath
       , output :: FilePath
       }
  deriving (Data, Typeable, Show, Eq)

repl = Repl &= help "Run the repl."            

ir = Ir
  { stage = enum
    [ Ast &= help "Produce a parse tree."
    , Elab &= help "Produce an elaboration."
    , Llvm &= help "Produce LLVM IR."
    ]
    , input = def &= typFile &= help "A bt source file."
    , output = def &= typFile &= help "Where to write the IR."
  } &= help "emits human readable IR"

processOptions :: IO Options
processOptions = cmdArgsRun $ cmdArgsMode $ modes [ repl, ir ]
  &= help "Jack, a toaster front-end"
  &= program "jack"
  &= summary "jack v0.1" -- FIXME: Need to get version number from config.
