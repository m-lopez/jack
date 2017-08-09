{-|
Module      : Repl.State
Description : A type for managing REPL state.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Repl.State (
  CompilerState(..)
) where 

import Context ( Ctx )

-- | Compiler state.
newtype CompilerState = CompilerState { getCtx :: Ctx }
