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
import Text.Parsec.Char ( oneOf, string, digit, char, letter, spaces, endOfLine )
import Text.Parsec.Combinator ( many1, sepBy, sepBy1, optionMaybe, many1 )
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
{- Notes
  One of the reasons that this parser is so complicated is that lexing logic is
  mingled with parsing logic. Not sure how to preserve the good parsec debug
  information if these phases are separated.
-}


replParse :: String -> DebugOr Ast
replParse code = case parse rule "REPL parser" code of
  Left x -> fail $ show x
  Right x -> mkSuccess x
  where
    rule = expr

parseModule :: String -> DebugOr Module
parseModule code = case parse module_ "Module parser" code of
  Left x -> fail $ show x
  Right x -> mkSuccess x

-- module ;;= [header] { top-level, ";" }
module_ :: Parser Module
module_ = do
  maybeHeader <- optionMaybe header
  tls <- sepBy toplevel $ reservedOp ";"
  eof
  return $ Module maybeHeader tls

-- header ::= "module" qualified "(" { identifier, "," }  ")"
header :: Parser Header
header = do
  keyword "module"
  moduleName <- qualified
  keyword "export"
  ops <- parens $ sepBy name $ reservedOp ","
  return $ Header moduleName ops

-- toplevel ::= import | type-definition | proposition-definition
--            | constant-definition
toplevel :: Parser TopLevel
toplevel =
  import_     <|>
  try typeDef <|>
  try propDef <|>
  constantDef <?>
  "top-level expression"

-- import ::= "import" qualified
import_ :: Parser TopLevel
import_ = do
  keyword "import"
  path <- qualified
  return $ Import path

-- type-definition ::= "type" identifier local-constant-context ":=" type
typeDef :: Parser TopLevel
typeDef = TypeDef <$>
  (keyword "type" *> name) <*>
  localConstantContext <*>
  (reservedOp ":=" *> type_)

-- proposition-definition ::=
--   "prop" identifier local-constant-context ":=" proposition
propDef :: Parser TopLevel
propDef = PropDef <$>
  (keyword "prop" *> name) <*>
  localConstantContext <*>
  (reservedOp ":=" *> prop)

-- constant-definition ::= identifier ":" local-context type ":=" expression
constantDef :: Parser TopLevel
constantDef = ConstantDef <$>
  name <*>
  (reservedOp ":" *> localContext) <*>
  type_ <*>
  (symbol ":=" *> expr)

-- local-constant-context ::= [ constant-parameters  ] [ proposition ]
localConstantContext :: Parser LocalConstantContext
localConstantContext = LocalConstantContext <$>
  (optionMaybe $ try constantParams) <*>
  (optionMaybe $ try prop)

-- local-context ::= [ constant-parameters ] [ proposition ] [ parameters ]
localContext :: Parser LocalContext
localContext = LocalContext <$>
  (optionMaybe $ try constantParams) <*>
  (optionMaybe $ try prop) <*>
  (optionMaybe $ try valueParams)

-- value-parameters ::= parameters "->"
valueParams :: Parser [Binding]
valueParams = parameters <* reservedOp "->"

-- constant-parameters ::= "[" { constant-binding, "," }  "]"
constantParams :: Parser [ConstantParameter]
constantParams = brackets $ sepBy constantBinding $ reservedOp ","

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
binding = (\x y -> (x,y)) <$> (name  <* reservedOp ":") <*> type_

type_ :: Parser Ast
type_ = try recType <|>
        try arrowType <|>
        identifier <|>
        builtinType <?>
        "type expression"

recType :: Parser Ast
recType = ARecType <$> braces bindings
  where
    bindings = sepBy binding $ symbol ","

arrowType :: Parser Ast
arrowType = AArrowType <$> parens types <* reservedOp "->" <*> type_
  where
    types = sepBy type_ $ symbol ","

builtinType :: Parser Ast
builtinType = ABuiltinType <$> (
  u8 <|> u16 <|> u32 <|> u64 <|>
  i8 <|> i16 <|> i32 <|> i64 <|>
  boolT <|>
  f32 <|> f64)
  where
    u8 = try (keyword "U8" *> return U8)
    u16 = try (keyword "U16" *> return U16)
    u32 = try (keyword "U32" *> return U32)
    u64 = try (keyword "U64" *> return U64)
    i8 = try (keyword "I8" *> return I8)
    i16 = try (keyword "I16" *> return I16)
    i32 = try (keyword "I32" *> return I32)
    i64 = try (keyword "I64" *> return I64)
    boolT = try (keyword "Bool" *> return BoolT)
    f32 = try (keyword "F32" *> return F32)
    f64 = try (keyword "F64" *> return F64)

prop :: Parser Ast
prop = fail "unsupported-01"

expr :: Parser Ast
expr = whiteSpace *> (try block <|> let_)

block :: Parser Ast
block = ABlock <$> (whiteSpace *> braces (sepBy let_ $ reservedOp ";"))

let_ :: Parser Ast
let_ = try letRule <|> if_
  where
    letRule = ALet <$> binding <*> (reservedOp ":=" *> if_) <*> let_

if_ :: Parser Ast
if_ = ifRule <|> binary
  where
    ifRule = AIf <$> (keyword "if" *> expr) <*> (keyword "then" *> expr) <*> (keyword "else" *> expr)

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
    preOp x = Prefix
      (reservedOp x *> return (\p -> AApp (AName [ x ]) [ p ]))
    binOpL x = Infix
      (reservedOp x *> return (\p1 p2 -> AApp (AName [ x ]) [ p1, p2 ]))
      AssocLeft

binary :: Parser Ast
binary = buildExpressionParser opTable application

-- arguments ::=  '(' [expr; ','] ')' | application
arguments :: Parser [Ast]
arguments = parens exprs <|> call_expr_as_args
  where
    call_expr_as_args  = pure <$> application :: Parser [Ast]
    exprs = sepBy expr $ symbol ","

-- application ::= simple arguments | simple
application :: Parser Ast
application = try (AApp <$> identifier <*> arguments) <|> simple <?> "application"

-- simple ::= '(' expr ')' | variable | literal
simple :: Parser Ast
simple = try (parens expr) <|> try identifier <|> try literal <?> "simple expression"

literal :: Parser Ast
literal = try bool <|> try hex <|> try floatingPoint <|> try integer <?> "literal"

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
identifier = AName . (: []) <$> name

name :: Parser String
name = T.identifier lexer

keyword :: String -> Parser ()
keyword = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

integer :: Parser Ast
integer = ALitInteger <$> T.natural lexer

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

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

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
          , "#", "$", "%", "^", "&", "|", "?", ";"
          ]

-- lexer :: GenTokenParser s u m
lexer = makeTokenParser lexDefs

