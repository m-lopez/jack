{-|
Module      : CodeGen.LLVM.Transformer
Description : Transform LLVM ASTs into other forms.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module CodeGen.LLVM.Transformer ( toReadableLlvmIr ) where

import CodeGen.LLVM.Ast ( toLlvmModuleAst )
import LLVM.Context ( withContext )
import LLVM.Module ( moduleLLVMAssembly, withModuleFromAST )



-- | Transform an LLVM.AST.Module into human-readable LLVM IR.
fromASTtoLlvmIr :: LLVM.AST.Module -> IO String
fromASTtoLlvmIr m = BS.unpack <$> (withContext $ fromModAndCtx m)
  where
    fromModAndCtx x y = withModuleFromAST y x moduleLLVMAssembly

-- | Attempt to translate a Toaster module into human-readable LLVM IR.
toReadableLlvmIr :: [TlExpr] -> DebugOr (IO String)
toReadableLlvmIr prgm = do
  ast <- toLlvmModuleAst
  return $ fromASTtoLlvmIr ast

  pre <- toPreLLVM prgm
  ast <- toLlvmModule pre
  return $ fromASTtoLlvmIr ast
