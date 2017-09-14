{-|
Module      : Syntax.Parser
Description : Types and operations for managing the elaboration context.
Copyright   : (c) Michael Lopez, 2017
License     : MIT
Maintainer  : m-lopez (github)
Stability   : unstable
Portability : non-portable
-}
module Syntax.Parser (
  replParse,
  parseModule
  ) where

import Syntax.Ast

import Util.DebugOr( DebugOr(..), fail, mkSuccess )
import Control.Applicative ((<*), (*>), (<|>), (<$>))
import Control.Monad (void)
import Data.Functor.Identity
import Text.Parsec (many, try, parse, (<?>), eof )
import Text.Parsec.Char (oneOf, string, digit, char, letter)
import Text.Parsec.Combinator ( many1, sepBy, sepBy1, optionMaybe )
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (
  buildExpressionParser,
  Operator(Prefix, Infix),
  Assoc(AssocLeft))
import Text.Parsec.Token (
  GenLanguageDef(..),
  makeTokenParser,
  GenTokenParser )
import qualified Text.Parsec.Token as T
import Data.Int ( Int32(..) )



replParse :: String -> DebugOr Ast
replParse code = case parse rule "REPL parser" code of
  Left x -> fail $ show x
  Right x -> mkSuccess x
  where
    rule = ast

parseModule :: String -> DebugOr Module
parseModule code = case parse module_ "Module parser" code of
  Left x -> fail $ show x
  Right x -> mkSuccess x

-- module ;;= [header] { top-level, ";" }
module_ :: Parser Module
module_ = Module <$> {-header <*>-} (sepBy toplevel $ symbol ";")

-- header ::= "module" qualified "(" { identifier, "," }  ")"
{-header :: Parser (Maybe Header)
header = optionMaybe $ try header
  where
    moduleKw = keyword "module"
    exportedSymbols = identifier `sepBy` (symbol ",")-}
    -- header = Header <$> moduleKw *> qualified <*> parens exportedSymbols

-- toplevel ::= import | type-definition | proposition-definition
--            | constant-definition
toplevel :: Parser TopLevel
toplevel =
  {-import_ <|>-}
  typeDef <|>
  propDef <|>
  constantDef <?>
  "top-level expression"

-- import ::= "import" qualified
-- import_ :: Parser TopLevel
-- import_ = Import <$> keyword "import" *> qualified

-- type-definition ::= "type" identifier local-constant-context ":=" type
typeDef :: Parser TopLevel
typeDef = TypeDef <$>
  (keyword "type" *> name) <*>
  localConstantContext <*>
  (reservedOp ":=" *> ast)

-- proposition-definition ::=
--   "prop" identifier local-constant-context ":=" proposition
propDef :: Parser TopLevel
propDef = PropDef <$>
  (keyword "prop" *> name) <*>
  localConstantContext <*>
  (reservedOp ":=" *> ast)

-- constant-definition ::= identifier ":" local-context type ":=" expression
constantDef :: Parser TopLevel
constantDef = ConstantDef <$>
  name <*>
  (symbol ":" *> localContext) <*>
  ast <*>
  (symbol ":=" *> ast)

-- local-constant-context ::= [ constant-parameters  ] [ proposition ]
localConstantContext :: Parser LocalConstantContext
localConstantContext = LocalConstantContext <$>
  (optionMaybe $ try constantParams) <*>
  (optionMaybe $ try ast)

-- local-context ::= [ constant-parameters ] [ proposition ] [ parameters ]
localContext :: Parser LocalContext
localContext = LocalContext <$>
  (optionMaybe $ try constantParams) <*>
  (optionMaybe $ try ast) <*>
  (optionMaybe $ try parameters)

-- constant-parameters ::= "[" { constant-binding, "," }  "]"
constantParams :: Parser [ConstantParameter]
constantParams = brackets $ sepBy constantBinding $ symbol ","

-- parameters ::= "(" { binding, "," }  ")"
parameters :: Parser [Binding]
parameters = parens $ sepBy binding $ symbol ","

-- constant-binding ::= identifier | binding
constantBinding :: Parser ConstantParameter
constantBinding = valueBinding <|> typeBinding <?> "constant binding"
  where
    typeBinding = try $ TypeParameter <$> name
    valueBinding = try $ ValueParameter <$> binding

-- binding ::= identifier ":" type
binding :: Parser Binding
binding = ((\x y -> (x,y)) <$> (name  <* symbol ":") <*> ast)

ast :: Parser Ast
ast = block

block :: Parser Ast
block = ABlock <$> braces (sepBy let_ $ symbol ";")

let_ :: Parser Ast
let_ = ALet <$>
  binding <*>
  (reservedOp ":=" *> if_) <*>
  let_

if_ :: Parser Ast
if_ = AIf <$>
  (keyword "if" *> ast) <*>
  (keyword "then" *> ast) <*>
  (keyword "else" *> ast)

opTable :: [[Operator String () Identity Ast]]
opTable = [
  -- Selection operator.
  [ binOpL "." ],
  -- Unary additive operators.
  [ preOp "-" ],
  -- Multiplicative operations.
  [ binOpL "*",
    binOpL "/",
    binOpL "rem" ],
  -- Additive operations.
  [ binOpL "+",
    binOpL "-" ],
  -- Comparison operations.
  [ binOpL "=",
    binOpL "<>",
    binOpL "<",
    binOpL "<=",
    binOpL ">",
    binOpL ">=" ],
  -- Unary logical operators.
  [ preOp "not" ],
  -- Multiplicative logical operators
  [ binOpL "and",
    binOpL "or"] ]
  where
    preOp x = 
      Prefix (reservedOp x *> return (\p -> AApp (AName [ x ]) [ p ]))
    binOpL x =
      Infix (reservedOp x *> return (\p1 p2 -> AApp (AName [ x ]) [ p1, p2 ])) AssocLeft

binary :: Parser Ast
binary = buildExpressionParser opTable application

-- arguments ::=  '(' [expr; ','] ')' | application
arguments :: Parser [Ast]
arguments = parens exprs <|> call_expr_as_args
  where
    call_expr_as_args  = pure <$> application :: Parser [Ast]
    exprs = sepBy ast $ symbol ","

-- application ::= simple arguments | simple
application :: Parser Ast
application = try (AApp <$> simple <*> arguments) <|> simple

-- simple ::= '(' expr ')' | variable | literal
simple :: Parser Ast
simple = parens ast <|> identifier <|> literal

literal :: Parser Ast
literal = bool <|> hex <|> floatingPoint <|> integer

bool :: Parser Ast
bool = ALitBoolean <$> (true <|> false) <?> "boolean literal"
  where
    true = try $ keyword "true" *> return True
    false = try $ keyword "false" *> return False

-- | A qualified symbol is one that is prepended by dot selections.
-- qualified ::= { identifier , "." }1
qualified :: Parser [AstName]
qualified = sepBy1 name $ symbol "."

identifier :: Parser Ast
identifier = AName . (\x -> [x]) <$> name

name :: Parser String
name = T.identifier lexer

keyword :: String -> Parser ()
keyword = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

integer :: Parser Ast
integer = ALitInteger <$> T.integer lexer

floatingPoint :: Parser Ast
floatingPoint = ALitDouble <$> T.float lexer

hex :: Parser Ast
hex = ALitInteger <$> T.hexadecimal lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

brackets :: Parser a -> Parser a
brackets = T.brackets lexer

braces :: Parser a -> Parser a
braces = T.braces lexer

symbol :: String -> Parser String
symbol = string

-- Create lexer functions for this language.
-- lexDefs :: GenLanguageDef s u m
lexDefs = LanguageDef
  { commentStart = "--{"
  , commentEnd = "}--"
  , commentLine = "--"
  , nestedComments = True
  , identStart = letter
  , identLetter = letter <|> digit <|> char '_'
  , opStart = oneOf symbolChars
  , opLetter = oneOf symbolChars
  , reservedNames = nms
  , reservedOpNames = ops
  , caseSensitive = True
  }
  where
    symbolChars = "!@#$%^&*-=+\\|<>.?~:"
    nms = [ "if", "else", "true", "false", "and", "or", "not", "rem"
          , "let", "unless", "in", "U8", "U16", "U32", "U64", "I8", "I16"
          , "I32", "I64", "Bool", "F32", "F63", "module"
          ]
    ops = [ "+", "-", "*", "/", "=", "=\\=", "<", "<=", ">", ">=", ":=", "@"
          , "#", "$", "%", "^", "&", "|", "?"
          ]

-- lexer :: GenTokenParser s u m
lexer = makeTokenParser lexDefs

