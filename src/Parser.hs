module Parser (
  Ast(..),
  AstName(..),
  replParse
  ) where

import Util.DebugOr( DebugOr(..), fail, mkSuccess )
import Control.Applicative ((<*), (*>), (<|>), (<$>))
import Control.Monad (void)
import Data.Functor.Identity
import Text.Parsec (many, try, parse)
import Text.Parsec.Char (oneOf, string, digit, char, letter)
import Text.Parsec.Combinator (many1, sepBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (
  buildExpressionParser,
  Operator(Prefix, Infix),
  Assoc(AssocLeft))



--------------------------------------------------------------------------------
--  Ast type
--
--  A type representing untyped, abstract syntax tree.

newtype AstName  = AstName String  deriving (Show, Eq)

-- | Abstract syntax trees for Toaster.
data Ast =
    ATypeBool
  | ATypeInt
  | AArrow    [Ast] Ast
  | ARecT     [(AstName, Ast)]
  | ALitBool  Bool
  | ALitInt   Integer
  | AName     AstName
  | AAbs      [(AstName, Ast)] Ast
  | AApp      Ast [Ast]
  | AIf       Ast Ast Ast
  | ACoerce   Ast Ast
  | AInit     [Ast]
  | ADef      AstName Ast Ast
  | ADecl     AstName Ast
  deriving (Show, Eq)

--------------------------------------------------------------------------------
--  Reserved tokens and string manipulation.

-- Reserved keywords.
reservedKeywords :: [String]
reservedKeywords = [ "if", "then", "else", "true", "false", "Bool", "Int",
                     "def", "and", "or", "not", "rem" ]

-- Reserved symbols.
reservedSymbols :: [String]
reservedSymbols = [ "->", "\\(", "(", ")", ",", ":=", ":", "=", "<>", "<", ">",
                     "<=", ">=" ]

-- Quote a string.
quote :: String -> String
quote s = "`" ++ s ++ "'"


--------------------------------------------------------------------------------
--  Auxiliary parser combinators

-- Eat whitespace.
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- Treat a parse rule as lexeme; eat trailing whitespace.
asLexeme :: Parser a -> Parser a
asLexeme p = p <* whitespace

-- Lex a string.
requireKeyword :: String -> Parser ()
requireKeyword s = case s `elem` reservedKeywords of
  True  -> void $ asLexeme $ string s
  False -> fail $ "alien keyword: " ++ quote s

-- Lex a symbol.
requireSymbol :: String -> Parser ()
requireSymbol s = case s `elem` reservedSymbols of
  True  -> void $ asLexeme $ string s
  False -> fail $ "alien symbol: " ++ quote s

-- Apply parse rule `p` between two symbols.
enclosed :: String -> Parser a -> String -> Parser a
enclosed open p close = do
  _   <- asLexeme $ string open
  ast <- asLexeme p
  _   <- asLexeme $ string close
  return ast



--------------------------------------------------------------------------------
--  Parse rules for bindings.

-- binding ::= identifier ":" arrowType
binding :: Parser (AstName, Ast)
binding = (\x y -> (x,y)) <$> identifier <* requireSymbol ":" <*> arrowType

-- bindings ::= [binding, ","]
bindings :: Parser [(AstName, Ast)]
bindings = sepBy binding (requireSymbol ",")



--------------------------------------------------------------------------------
--  Parse rules for expressions.

expr :: Parser Ast
expr = lambda

-- exprs ::= [expr; ","]
exprs :: Parser [Ast]
exprs = sepBy (asLexeme expr) (requireSymbol ",")

-- lambda ::= '\(' bindings ')' expr
lambda :: Parser Ast
lambda = try abs_e <|> conditional
  where
    abs_head = requireSymbol "\\(" *> bindings <* requireSymbol ")"
    abs_e    = AAbs <$> abs_head <*> lambda

-- conditional ::= 'if' expr 'then' expr 'else' expr
--               | binary
conditional :: Parser Ast
conditional = try if_then_else_expr <|> binary
  where
    if_then_else_expr :: Parser Ast
    if_then_else_expr = do
      _    <- requireKeyword "if"
      ast1 <- expr
      _    <- requireKeyword "then"
      ast2 <- expr
      _    <- requireKeyword "else"
      ast3 <- expr
      return $ AIf ast1 ast2 ast3

opTable :: [[Operator String () Identity Ast]]
opTable = [
  -- Unary additive operators.
  [ pre_op "-" ],
  -- Multiplicative operations.
  [ bin_opl "*",
    bin_opl "/",
    bin_opl "rem" ],
  -- Additive operations.
  [ bin_opl "+",
    bin_opl "-" ],
  -- Comparison operations.
  [ bin_opl "=",
    bin_opl "<>",
    bin_opl "<",
    bin_opl "<=",
    bin_opl ">",
    bin_opl ">=" ],
  -- Unary logical operators.
  [ pre_op "not" ],
  -- Multiplicative logical operators
  [ bin_opl "and",
    bin_opl "or"] ]
  where
    as_app1 sym p = AApp (AName $ AstName sym) [ p ]
    pre_op sym = Prefix (as_app1 sym <$ try (asLexeme $ string sym))
    as_app2 sym p1 p2 = AApp (AName $ AstName sym) [ p1, p2 ]
    bin_opl sym = Infix (as_app2 sym <$ try (asLexeme $ string sym)) AssocLeft

binary :: Parser Ast
binary = buildExpressionParser opTable application

-- arguments ::=  '(' [expr; ','] ')' | application
arguments :: Parser [Ast]
arguments = enclosed "(" exprs ")" <|> call_expr_as_args
  where call_expr_as_args  = pure <$> application :: Parser [Ast]

-- application ::= simple arguments | simple
application :: Parser Ast
application = try (AApp <$> simple <*> arguments) <|> simple

-- simple ::= '(' expr ')' | variable | literal
simple :: Parser Ast
simple = enclosed "(" expr ")" <|> variable <|> literal

identifier :: Parser AstName
identifier = do
    n <- asLexeme ((:) <$> letter <*> many varChar)
    _ <- require_not_reserved n
    return $ AstName n
  where
    varChar = digit <|> letter <|> char '_'
    require_not_reserved s = case s `elem` reservedKeywords || s `elem` reservedSymbols of
      True  -> fail $ "expected identifier; got keyword " ++ quote s
      False -> return ()

variable :: Parser Ast
variable = try (AName <$> identifier)

boolLit :: Parser Bool
boolLit = true <|> false
  where
    true  = return True <* requireKeyword "true"
    false = return False <* requireKeyword "false"

intLit :: Parser Integer
intLit = read <$> many1 digit

literal :: Parser Ast
literal = asLexeme $ int <|> bool
  where
    int  = ALitInt <$> intLit
    bool = ALitBool <$> boolLit



--------------------------------------------------------------------------------
--  Parse rules for types.

-- | A parser for a type.
typ :: Parser Ast
typ = arrowType

-- | a parser for a sequence of types separated by a comma.
types :: Parser [Ast]
types = sepBy typ (requireSymbol ",")

-- literalType ::= Bool | Integer
literalType :: Parser Ast
literalType = bool_type <|> int_type
  where
    bool_type = return ATypeBool <* requireKeyword "Bool"
    int_type  = return ATypeInt  <* requireKeyword "Int"

-- recType ::= "{" bindings "}"
recType :: Parser Ast
recType = ARecT <$> enclosed "{" bindings "}"

-- baseType ::= (arrowType) | var | literalType
baseType :: Parser Ast
baseType =
  enclosed "(" arrowType ")" <|> recType <|> literalType <|> variable

-- arrowType ::= "(" types ")" -> arrowType
--              | baseType -> arrowType
--              | baseType
arrowType :: Parser Ast
arrowType = try (AArrow <$> arrow_head <*> arrowType)
         <|> try (AArrow <$> simple_head <*> arrowType)
         <|> baseType
  where
    arrow_head  = enclosed "(" types ")" <* requireSymbol "->"
    simple_head = pure <$> baseType <* requireSymbol "->" :: Parser [Ast]

-- | A parse rule for unquantified definitions.
def :: Parser Ast
def = mkDef <$> def_head <*> expr
  where
    def_head = binding <* requireSymbol ":="
    mkDef :: (AstName, Ast) -> Ast -> Ast
    mkDef = uncurry ADef

-- | A parse rule for declarations.
decl :: Parser Ast
decl = uncurry ADecl <$> binding

-- | A parser for the REPL.
replParse :: String -> DebugOr Ast
replParse code = case parse rule "REPL parser" code of
  Left x -> fail $ show x
  Right x -> mkSuccess x
  where
    rule = try def <|> try decl <|> expr
