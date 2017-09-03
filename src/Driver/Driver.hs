{-|
Module      : Driver.Driver
Description : A driver for the front-end.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Driver.Driver ( exec ) where

import Repl ( repl )
import Driver.Options ( Options(..), processOptions )
import Driver.Ir ( writeIr )

-- | Entrypoint into the front-end.
exec :: IO ()
exec = do
  opts <- processOptions
  case opts of
    Repl -> repl
    Ir stage input output -> writeIr stage input output
