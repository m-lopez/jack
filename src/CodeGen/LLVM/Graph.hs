{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : CodeGen.LLVM.Graph
Description : A control-flow graph for building LLVM definition bodies.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module CodeGen.LLVM.Graph (
  toLlvmType,
  localRef,
  globalRef,
  toName,
  SymbolTable,
  fromBindings,
  topLevelSymbols,
  CfgGen,
  bodyBuilder,
  startBlock,
  termBlock,
  currentBlockName,
  getVar,
  pushInst
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified LLVM.AST as L
import qualified LLVM.AST.Constant as L
import qualified LLVM.AST.Instruction as L
import qualified LLVM.AST.Operand as L
import Control.Monad.State ( State, MonadState, gets, modify, execState )
import Data.ByteString.Short ( toShort, ShortByteString )
import Expressions ( ExprName(..), QType(..), CType(..), TlExpr(..) )



-- Auxiliary -------------------------------------------------------------------

-- | Transform a Toaster type into an LLVM type.
toLlvmType :: CType -> L.Type
toLlvmType t = case t of
  CTBool      -> L.IntegerType 32
  CTI32       -> L.IntegerType 32
  CTArrow _ _ -> L.IntegerType 32
  CTVar _     -> L.IntegerType 32

-- | Construct a reference to a local parameter.
localRef :: String -> CType -> L.Operand
localRef x ct = L.LocalReference t name
  where
    t = toLlvmType ct
    name = toName x

-- | Construct a reference to a global out of a Toaster binding.
globalRef :: String -> CType -> L.Operand
globalRef x ct = L.ConstantOperand $ L.GlobalReference t name
  where
    t = toLlvmType ct
    name = toName x

-- | Transform a `String` to a `ShortByteString`.
toShortString :: String -> ShortByteString
toShortString = toShort . BS.pack

-- | Transform a string into a name.
toName :: String -> L.Name
toName = L.Name . toShortString



-- Symbol Tables ---------------------------------------------------------------

-- | Symbol tables map variable names with LLVM and type information.
type SymbolTable = Map.Map String (CType, L.Operand)

-- | Converts bindings into a symbol table.
fromBindings :: [(String, CType)] -> SymbolTable
fromBindings bs = Map.fromList $ map toSymbolEntry bs
  where
    toSymbolEntry (x, t) = (x, (t, localRef x t))

-- | Build a symbol table for all of the top-level definitions.
topLevelSymbols :: [TlExpr] -> SymbolTable
topLevelSymbols tls = Map.fromList $ map enterSymbol tls
  where
    enterSymbol tl = case tl of
      TlDef x (Unquantified t) _ -> (x, (t, globalRef x t))
      TlDef _ Quantified {} _ ->
        fail "found quantified definition at top-level"
      TlFuncDef x bs resT _ -> (x, (t, globalRef x resT))
        where t = CTArrow (map snd bs) resT



-- Control-flow Graphs ---------------------------------------------------------

-- | The current state of a block as it is built.
data BlockState = BlockState {
  name :: L.Name,
  instructions :: [ L.Named L.Instruction ] }

-- | State of a contol flow graph.
data CfgState = CfgState {
  symbols :: SymbolTable,
  varCount :: Word,
  blockCount :: Word,
  working :: [ BlockState ],
  finished :: [ L.BasicBlock ] }

-- | Caps off a Cfg with a toasty return statement.
finishCfg :: CfgState -> L.Operand -> [L.BasicBlock]
finishCfg cfg o = toBasicBlock z b : bs
  where
    bs = finished cfg
    b = (head . working) cfg
    z = L.Do $ L.Ret (Just o) []

-- | Construct a `L.BasicBlock` from `BlockState`.
toBasicBlock :: L.Named L.Terminator -> BlockState -> L.BasicBlock
toBasicBlock term (BlockState n is) = L.BasicBlock n si term where
  si = reverse is

-- | An initial state of a CFG.
initCfgState :: SymbolTable -> CfgState
initCfgState symbols = CfgState symbols 0 0 [ BlockState nm [] ] [ ]
  where
    nm = L.Name "entry"

-- | Raise `CfgState` into `MonadState`.
newtype CfgGen a = CfgGen { runCfgGen :: State CfgState a }
  deriving (Functor, Applicative, Monad, MonadState CfgState )

-- | Pop the current working  block.
popWorking :: CfgGen BlockState
popWorking = do
  b <- gets (head . working)
  modify $ \s -> s { working = tail $ working s }
  return b

-- | Push an instruction into the current CFG.
pushIntoBlock :: L.Named L.Instruction -> CfgGen ()
pushIntoBlock i = do
  b <- popWorking
  let b' = b { instructions = i : instructions b }
  modify $ \s -> s { working = b' : working s }

-- | Generates a new block name.
genBlockName :: CfgGen L.Name
genBlockName = do
  n <- gets blockCount
  modify $ \s -> s { blockCount = n + 1 }
  return $ toName $ "block" ++ show n

-- | Create a fresh variable name.
genSym :: CfgGen L.Name
genSym = do
  n <- gets varCount
  modify $ \s -> s { varCount = n + 1 }
  return $ L.UnName n

-- | Lookup a variable from the symbol table.
getVar :: String -> CfgGen L.Operand
getVar x = do
  symTbl <- gets symbols
  case Map.lookup x symTbl of
    Just (_, o) -> return o
    Nothing -> fail $ "failed to find variable `" ++ x ++ "`"

-- | Terminates the current block.
termBlock :: L.Named L.Terminator -> CfgGen ()
termBlock z = do
  b <- popWorking
  let b' = toBasicBlock z b
  modify $ \s -> s { finished = b' : finished s } -- push finished block

-- | Start a new block.
startBlock :: CfgGen L.Name
startBlock = do
  n <- genBlockName
  let b = BlockState n []
  modify $ \s -> s { working = b : working s }
  return n

-- | Retrieve the name of the current working block.
currentBlockName :: CfgGen L.Name
currentBlockName = gets (name . head . working)

-- | Push an instruction into the current block and generate a new variable.
pushInst :: L.Instruction -> L.Type -> CfgGen L.Operand
pushInst inst t = do
  sym <- genSym
  pushIntoBlock (sym L.:= inst)
  return $ L.LocalReference t sym

-- | Given a completed control-flow graph `g` and a symbol table `syms`, compute
-- a list of `L.BasicBlock`s.
bodyBuilder :: SymbolTable -> CfgGen () -> [L.BasicBlock]
bodyBuilder syms g = finished $ execState prgm $ initCfgState syms
  where
    prgm = runCfgGen g
