module Parser ( Ast (..)
              , Ast_name (..)
              , repl_parse
              , simple_parse) where

import Util.DebugOr( DebugOr(..), fail, mk_success )

import Control.Applicative ((<*), (*>), (<|>), (<$>))
import Control.Monad (void)

import Data.Functor.Identity

import Text.Parsec (many, try, parse)
import Text.Parsec.Char (oneOf, string, digit, char, letter)
import Text.Parsec.Combinator (many1, sepBy, sepBy1)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr ( buildExpressionParser
                        , Operator(Prefix, Infix)
                        , Assoc(AssocLeft))



--------------------------------------------------------------------------------
--  Ast type
--
--  A type representing untyped, abstract syntax tree.

newtype Ast_name  = Ast_name String  deriving (Show, Eq)

data Ast =
    A_type_bool
  | A_type_int
  | A_arrow     [Ast] Ast
  | A_rec_t     [(Ast_name, Ast)]
  | A_lit_bool  Bool
  | A_lit_int   Integer
  | A_name      Ast_name
  | A_abs       [(Ast_name, Ast)] Ast
  | A_app       Ast [Ast]
  | A_if        Ast Ast Ast
  | A_coerce    Ast Ast
  | A_init      [Ast]
  | A_def       (Ast_name, Ast) Ast
  deriving (Show, Eq)



--------------------------------------------------------------------------------
--  Reserved tokens and string manipulation.

-- Reserved keywords.
reserved_keywords :: [String]
reserved_keywords = [ "if", "then", "else", "true", "false", "Bool", "Int",
                      "def", "and", "or", "not", "rem" ]

-- Reserved symbols.
reserved_symbols :: [String]
reserved_symbols = [ "->", "\\(", "(", ")", ",", ":=", ":", "=", "<>", "<", ">",
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
as_lexeme :: Parser a -> Parser a
as_lexeme p = p <* whitespace

-- Lex a string.
require_keyword :: String -> Parser ()
require_keyword s = case s `elem` reserved_keywords of
  True  -> void $ as_lexeme $ string s
  False -> fail $ "alien keyword: " ++ quote s

-- Lex a symbol.
require_symbol :: String -> Parser ()
require_symbol s = case s `elem` reserved_symbols of
  True  -> void $ as_lexeme $ string s
  False -> fail $ "alien symbol: " ++ quote s

-- Apply parse rule `p` between two symbols.
enclosed :: String -> Parser a -> String -> Parser a
enclosed open p close = do
  _   <- as_lexeme $ string open
  ast <- as_lexeme $ p
  _   <- as_lexeme $ string close
  return ast



--------------------------------------------------------------------------------
--  Parse rules for bindings.

-- binding ::= identifier ":" arrow_type
binding :: Parser (Ast_name, Ast)
binding = (\x y -> (x,y)) <$> identifier <* (require_symbol ":") <*> arrow_type

-- bindings ::= [binding, ","]
bindings :: Parser [(Ast_name, Ast)]
bindings = sepBy binding (require_symbol ",")



--------------------------------------------------------------------------------
--  Parse rules for expressions.

expr :: Parser Ast
expr = lambda

-- exprs ::= [expr; ","]
exprs :: Parser [Ast]
exprs = sepBy (as_lexeme expr) (require_symbol ",")

-- lambda ::= '\(' bindings ')' expr
lambda :: Parser Ast
lambda = (try abs_e) <|> conditional
  where
    abs_head = (require_symbol "\\(") *> bindings <* (require_symbol ")")
    abs_e    = A_abs <$> abs_head <*> lambda

-- conditional ::= 'if' expr 'then' expr 'else' expr
--               | binary
conditional :: Parser Ast
conditional = try (if_then_else_expr) <|> binary
  where
    if_then_else_expr :: Parser Ast
    if_then_else_expr = do
      _    <- require_keyword "if"
      ast1 <- expr
      _    <- require_keyword "then"
      ast2 <- expr
      _    <- require_keyword "else"
      ast3 <- expr
      return $ A_if ast1 ast2 ast3

op_table :: [[Operator String () Identity Ast]]
op_table = [
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
    as_app1 sym p = A_app (A_name $ Ast_name sym) [ p ]
    pre_op sym = Prefix (as_app1 sym <$ (try (as_lexeme $ string sym)))
    as_app2 sym p1 p2 = A_app (A_name $ Ast_name sym) [ p1, p2 ]
    bin_opl sym = Infix (as_app2 sym <$ (try (as_lexeme $ string sym))) AssocLeft

binary :: Parser Ast
binary = buildExpressionParser op_table application

-- arguments ::=  '(' [expr; ','] ')' | application
arguments :: Parser [Ast]
arguments = (enclosed "(" exprs ")") <|> call_expr_as_args
  where call_expr_as_args  = pure <$> application :: Parser [Ast]

-- application ::= simple arguments | simple
application :: Parser Ast
application = try (A_app <$> simple <*> arguments) <|> simple

-- simple ::= '(' expr ')' | variable | literal
simple :: Parser Ast
simple = (enclosed "(" expr ")") <|> variable <|> literal

identifier :: Parser Ast_name
identifier = do
    n <- as_lexeme ((:) <$> letter <*> many varChar)
    _ <- require_not_reserved n
    return $ Ast_name n
  where
    varChar = digit <|> letter <|> char '_'
    require_not_reserved s = case s `elem` reserved_keywords || s `elem` reserved_symbols of
      True  -> fail $ "expected identifier; got keyword " ++ quote s
      False -> return ()

variable :: Parser Ast
variable = try (A_name <$> identifier)

bool_lit :: Parser Bool
bool_lit = true <|> false
  where
    true  = return True <* require_keyword "true"
    false = return False <* require_keyword "false"

int_lit :: Parser Integer
int_lit = read <$> many1 digit

literal :: Parser Ast
literal = as_lexeme $ (int <|> bool)
  where
    int  = A_lit_int <$> read <$> many1 digit
    bool = A_lit_bool <$> bool_lit



--------------------------------------------------------------------------------
--  Parse rules for types.

-- types ::= [arrow_type; ","]
types :: Parser [Ast]
types = sepBy arrow_type (require_symbol ",")

-- literal_type ::= Bool | Integer
literal_type :: Parser Ast
literal_type = bool_type <|> int_type
  where
    bool_type = return A_type_bool <* require_keyword "Bool"
    int_type  = return A_type_int  <* require_keyword "Int"

-- rec_type ::= "{" bindings "}"
rec_type :: Parser Ast
rec_type = A_rec_t <$> enclosed "{" bindings "}"

-- base_type ::= (arrow_type) | var | literal_type
base_type :: Parser Ast
base_type =
  (enclosed "(" arrow_type ")") <|> rec_type <|> literal_type <|> variable

-- arrow_type ::= "(" types ")" -> arrow_type
--              | base_type -> arrow_type
--              | base_type
arrow_type :: Parser Ast
arrow_type = try (A_arrow <$> arrow_head <*> arrow_type)
         <|> try (A_arrow <$> simple_head <*> arrow_type)
         <|> base_type
  where
    arrow_head  = (enclosed "(" types ")") <* (require_symbol "->")
    simple_head = (pure <$> base_type) <* (require_symbol "->") :: Parser [Ast]



--------------------------------------------------------------------------------
--  Parse rules for definitions.

def :: Parser Ast
def = A_def <$> def_head <*> expr
  where
    def_head = (require_keyword "def") *> binding <* (require_symbol ":=")



--------------------------------------------------------------------------------
--  A REPL parser.
--  FIXME: Guard against EOF.
repl_parse :: Parser Ast
repl_parse = def <|> arrow_type <|> expr

-- FIXME: This is not thought out.
simple_parse :: String -> DebugOr Ast
simple_parse s = case parse expr "simple_expression_parser" s of
  Left x  -> fail $ show x
  Right x -> mk_success x
