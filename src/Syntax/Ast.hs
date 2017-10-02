{-|
Module      : Syntax.Parser
Description : Types and operations for managing the elaboration context.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Syntax.Ast (
  Ast(..),
  AstName,
  QualifiedAstName,
  Module(..),
  Header(..),
  TopLevel(..),
  LocalContext(..),
  ConstantParameter(..),
  LocalConstantContext(..),
  BuiltinType(..),
  Binding(..),
  ObjectModifier(..) ) where



-- | A type for symbols in Asts.
type AstName = String
type QualifiedAstName = [String]

-- | The AST for a Toaster module.
data Module = Module (Maybe Header) [TopLevel]

instance Show Module where
  show (Module Nothing tls) = "Module " ++ show tls
  show (Module (Just hdr) tls) = "Module (" ++ show hdr ++ ") " ++ show tls

-- | List of symbols to export.
data Header = Header QualifiedAstName [AstName]
  deriving ( Show, Eq )

-- | Top-level definitions.
data TopLevel =
  Import QualifiedAstName |
  TypeDef AstName LocalConstantContext Ast |
  PropDef AstName LocalConstantContext Ast |
  ConstantDef AstName LocalContext Ast Ast
  deriving ( Show, Eq )

-- | Parameters overwhich a definition is abstracted.
data LocalContext = LocalContext
  { constParams :: Maybe [ConstantParameter]  -- parametric
  , constraint :: Maybe Ast                   -- constrained
  , params :: Maybe [Binding]                 -- function
  } deriving ( Show, Eq )

-- | Constant parameter.
data ConstantParameter =
  TypeParameter AstName |
  ValueParameter Binding
  deriving ( Show, Eq )

-- | Constant parameters overwhich a definition can be abstracted.
data LocalConstantContext = LocalConstantContext
  { constParams_ :: Maybe [ConstantParameter]
  , constraint_ :: Maybe Ast
  } deriving ( Show, Eq )


-- | Abstract syntax trees for Toaster.
data Ast =
  ABuiltinType BuiltinType |
  ARecType [Binding] |
  AArrowType [(ObjectModifier, Ast)] Ast |
  ALitBoolean Bool |
  ALitInteger Integer |
  ALitDouble Double |
  AName AstName |
  AAbs [Binding] Ast |
  AApp Ast [Ast] |
  AIf Ast Ast Ast |
  ADot AstName Ast |
  ARecInit [(Binding,Ast)] |
  ALet Binding Ast Ast |
  ABlock [Ast] |
  AWhile Ast Ast
  deriving (Show, Eq)

-- | Builtin data types.
data BuiltinType =
  U8 | U16 | U32 | U64 |  -- unsigned integer types
  I8 | I16 | I32 | I64 |  -- signed integer types
  BoolT |                 -- Boolean
  VoidT |                 -- Void
  F32 | F64               -- IEEE 754 floating-point
  deriving ( Show, Eq )

-- | A name-type binding.
data Binding = Binding AstName ObjectModifier Ast
  deriving ( Show, Eq )

-- | Object type modifier. Modifies how a stack object is treated.
data ObjectModifier = Constant | Mutable deriving ( Show, Eq )

