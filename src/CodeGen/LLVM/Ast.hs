{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : CodeGen.LLVM.Ast
Description : Construct an LLVM IR Ast.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module CodeGen.LLVM.Ast ( toLlvmModuleAst ) where

import qualified Data.Map.Strict as Map
import qualified LLVM.AST as A
import qualified LLVM.AST.CallingConvention as L
import qualified LLVM.AST.Constant as L
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Name as L
import qualified LLVM.AST.Operand as L
import qualified LLVM.AST.Type as T
import CodeGen.LLVM.Prep ( prep )
import CodeGen.LLVM.Graph
import Data.Int ( Int32 )
import Data.Maybe ( catMaybes, fromJust )
import Expressions ( ExprName(..), Expr(..), CType(..), QType(..), TlExpr(..) )
import Util.DebugOr ( DebugOr, mkSuccess )

-- FIXME: Need a better technique for errors.
unsupported :: String -> a
unsupported msg = error $ "sorry, unimplemented: " ++ msg
ice :: String -> a
ice msg = error $ "ICE: " ++ msg

-- | True iff an expression is representable as an LLVM constant.
isSimpleLiteral :: Expr -> Bool
isSimpleLiteral e = case e of
  ELitBool {} -> True
  ELitInt {} -> True
  EVar {} -> False
  EAbs {} -> False
  EApp {} -> False
  EIf {} -> False
  EUnBuiltin {} -> False
  EBinBuiltin {} -> False

-- | Build a Boolean literal.
bool :: Bool -> L.Operand
bool b = L.ConstantOperand $ L.Int 32 n
  where
    n = if b then 1 else 0

-- | Build an integer literal.
int :: Int32 -> L.Operand
int i = L.ConstantOperand $ L.Int 32 $ toInteger i

-- | Construct a load instruction.
load :: L.Operand -> CfgGen L.Operand
load o = pushInst (I.Load False o Nothing 0 []) (T.IntegerType 32)

-- | True iff the operand is a global reference.
isGlobalRef :: L.Operand -> Bool
isGlobalRef o = case o of
  L.ConstantOperand L.GlobalReference {} -> True
  _ -> False

-- | Build a variable access.
var :: String -> CfgGen L.Operand
var x = do
  o <- getVar x
  if isGlobalRef o then load o else return o



-- | Look-up a built-in unary operation by ID.
getUnary :: String -> L.Operand -> (I.Instruction, T.Type)
getUnary id = fromJust $ lookup id insts
  where
    insts = [
      -- This shows twice use of C.Int. Might also want a zero constant operand.
      ("not_Bool", \x -> (I.Xor x (bool False) [], T.IntegerType 32)) ]

-- | Construct a unary operation.
unary :: String -> [L.Operand] -> CfgGen L.Operand
unary x ops = case ops of
  [ o ] -> uncurry pushInst $ getUnary x o
  _ -> ice $ "unary operation `" ++ x ++ "` called with " ++ (show $ length ops) ++ " arguments"

-- | Look-up a built-in binary operation by ID.
getBin :: String -> L.Operand -> L.Operand -> (I.Instruction, T.Type)
getBin id = fromJust $ lookup id insts
  where
    insts = [
      ("+_I32", \x y -> (I.Add False False x y [], T.IntegerType 32)),
      ("-_I32", \x y -> (I.Sub False False x y [], T.IntegerType 32)),
      ("*_I32", \x y -> (I.Mul False False x y [], T.IntegerType 32)) ]

-- | Construct a binary operation.
binary :: String -> [L.Operand] -> CfgGen L.Operand
binary x ops = case ops of
  [ o1, o2 ] -> uncurry pushInst $ getBin x o1 o2
  _ -> ice $ "binary operation `" ++ x ++ "` called with " ++ (show $ length ops) ++ " arguments"

-- | Built a function call.
call :: String -> QType -> [ L.Operand ] -> CfgGen L.Operand
call x (Unquantified (CTArrow argTs resT)) ops | length argTs == length ops = do
  let t = toLlvmType resT
  let f = globalRef x resT
  let i = I.Call Nothing L.C [] (Right f) (map (\x -> (x, [])) ops) [] []
  pushInst i t
call _ _ _ = fail "got a call that expected different arguments"

-- | Translate an application into either an instruction or a call.
apply :: Expr -> [Expr] -> CfgGen L.Operand
apply f args = do
  ops <- mapM toSsa args
  case f of
    EUnBuiltin _ x _ -> unary x ops
    EBinBuiltin _ x _ -> binary x ops
    EVar (ExprName x) t -> call x t ops
    e -> ice $ "caller " ++ show e ++ "cannot be mapped to SSA"

-- | Build a block from an expression and a function that builds a terminator
-- from an operand.
-- FIXME: What about the trivial case where this is a constant?
block :: Expr -> (L.Operand -> I.Named I.Terminator) -> CfgGen (L.Name, L.Operand)
block e mkTerm = do
  n <- startBlock
  o <- toSsa e
  termBlock $ mkTerm o
  return (n,o)

-- | Construct a branching expression.
branch :: Expr -> Expr -> Expr -> CfgGen L.Operand
branch c e1 e2 = do
  current <- currentBlockName
  (b1, o1) <- block e1 (const $ I.Do $ I.Br current [])
  (b2, o2) <- block e2 (const $ I.Do $ I.Br current [])
  (_, o) <- block c (\x  -> I.Do $ I.CondBr x b1 b2 [])
  pushInst (I.Phi (T.IntegerType 32) [ (o1, b1), (o2, b2) ] []) (T.IntegerType 32)

-- | Transform an expression into a CFG that writes to an operand.
toSsa :: Expr -> CfgGen L.Operand
toSsa e = case e of
  ELitBool b -> return $ bool b
  ELitInt n -> return $ int n
  EVar (ExprName x) t -> var x
  EAbs _ _ -> ice "found an asbtraction while translating to SSA"
  EApp f args -> apply f args
  EIf e1 e2 e3 -> branch e1 e2 e3
  EUnBuiltin {} -> ice "found a built-in operator while translating to SSA"
  EBinBuiltin {} -> ice "found a built-in operator while translating to SSA"

-- | Transform the body of a definition to SSA form.
bodyToSsa :: Expr -> CfgGen ()
bodyToSsa e = do
  op <- toSsa e
  let z = I.Do $ I.Ret (Just op) []
  termBlock z

-- | Generate definitions from the top-level.
genDefs :: SymbolTable -> [TlExpr] -> [Maybe A.Definition]
genDefs symbols = map genDef
  where
    genDef tl = case tl of
      TlFuncDef x bs t e -> let
          localSymbols = Map.union (fromBindings bs) symbols
        in Just $ A.GlobalDefinition $ mkSimpleFunction bs localSymbols x e
      TlDef x (Unquantified ct) e  ->
        Just $ A.GlobalDefinition $ mkSimpleGlobalVar symbols x e
      TlDef _ Quantified {} _ -> Nothing

toInit :: Expr -> Maybe L.Constant
toInit e = case e of
  ELitBool b -> if b then Just $ L.Int 32 1 else Just $ L.Int 32 0
  ELitInt n -> Just $ L.Int 32 $ toInteger n
  _ -> Nothing

-- | Make a simple variable declaration.
mkSimpleGlobalVar :: SymbolTable -> String -> Expr -> G.Global
mkSimpleGlobalVar symbols nm e = G.globalVariableDefaults {
  G.name = toName nm,
  G.linkage = L.Private,
  G.unnamedAddr = Just G.GlobalAddr,
  G.type' = T.IntegerType 32,
  G.isConstant = True,
  G.initializer = if isSimpleLiteral e then toInit e else unsupported "complex variable initializations" }

-- | Convert Toaster bindings into LLVM parameters.
toParams ::[(String, CType)] -> [G.Parameter]
toParams = map toParam
  where
    toParam (x, ct) = G.Parameter (toLlvmType ct) (toName x) []

-- | Make a simple function.
mkSimpleFunction :: [(String, CType)] -> SymbolTable -> String -> Expr -> G.Global
mkSimpleFunction bindings symbols nm e = G.functionDefaults {
  G.linkage = if nm == "main" then L.External else L.Private,
  G.returnType = T.IntegerType 32,
  G.name = toName nm,
  G.parameters = (toParams bindings, False),
  G.basicBlocks = bodyBuilder symbols $ bodyToSsa e }

-- | Translates an program to an `LLVM.AST.Module`.
toLlvmModuleAst :: [TlExpr] -> DebugOr A.Module
toLlvmModuleAst tls = do
  prepped <- prep tls
  let symbols = topLevelSymbols tls
  let defs = catMaybes $ genDefs symbols prepped
  return A.defaultModule {
    A.moduleName = "toaster-module",
    A.moduleDefinitions = defs }
