module Parser (Ast (..), Ast_name (..), repl_parse) where



import Control.Applicative ((<*), (*>), (<|>), (<$>))
import Control.Monad (void)

import Text.Parsec (many, try)
import Text.Parsec.Char (oneOf, string, digit, char, letter)
import Text.Parsec.Combinator (many1, sepBy, sepBy1)
import Text.Parsec.String (Parser)



--------------------------------------------------------------------------------
--  Ast type
--
--  A type representing untyped abstract syntax tree.

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
  deriving (Show)



--------------------------------------------------------------------------------
--  Reserved tokens and string manipulation.

-- Reserved keywords.
reserved_keywords :: [String]
reserved_keywords = [ "if", "then", "else", "true", "false", "Bool", "Int",
                      "def" ]

-- Reserved symbols.
reserved_symbols :: [String]
reserved_symbols = [ "->", "\\(", "(", ")", ",", ":=", ":" ]

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
--
--  A parser implemented using the Parsec library.

-- FIXME: Don't allow keywords.
identifier :: Parser Ast_name
identifier = do
    n <- as_lexeme ((:) <$> letter <*> many varChar)
    _ <- require_not_keyword n
    return $ Ast_name n
  where
    varChar = digit <|> letter <|> char '_'
    require_not_keyword s = case s `elem` reserved_keywords of
      True  -> fail $ "expected identifier; got keyword " ++ quote s
      False -> return ()

var :: Parser Ast
var = try (A_name <$> identifier)

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

-- exprs ::= [expr; ","]
exprs :: Parser [Ast]
exprs = sepBy (as_lexeme expr) (require_symbol ",")

-- rec_expr ::= "{" exprs "}"
rec_expr :: Parser Ast
rec_expr = A_init <$> enclosed "{" exprs "}"

-- base_expr ::= (expr) | rec_expr | var | literal
base_expr :: Parser Ast
base_expr = (enclosed "(" expr ")") <|> rec_expr <|> var <|> literal

-- args_expr ::=  (e_1, ..., e_n) | call_expr
args_expr :: Parser [Ast]
args_expr = (enclosed "(" exprs ")") <|> call_expr_as_args
  where
    call_expr_as_args  = pure <$> call_expr :: Parser [Ast]

--  call_expr ::= base_expr args_expr | base_expr
call_expr :: Parser Ast
call_expr = try (A_app <$> base_expr <*> args_expr) <|> base_expr

-- cond_expr ::= if cond_expr then cond_expr else cond_expr | call_expr
cond_expr :: Parser Ast
cond_expr = try (if_then_else_expr) <|> call_expr
  where
    if_then_else_expr :: Parser Ast
    if_then_else_expr = do
      _    <- require_keyword "if"
      ast1 <- cond_expr
      _    <- require_keyword "then"
      ast2 <- cond_expr
      _    <- require_keyword "else"
      ast3 <- cond_expr
      return $ A_if ast1 ast2 ast3

-- lambda_expr ::= "\(" bindings ")" cond_expr
lambda_expr :: Parser Ast
lambda_expr = (try abs_e) <|> cond_expr
  where
    abs_head = (require_symbol "\\(") *> bindings <* (require_symbol ")")
    abs_e    = A_abs <$> abs_head <*> lambda_expr

-- coerce_expr ::= cond_expr @ type | cond_expr
coerce_expr :: Parser Ast
coerce_expr = try (A_coerce <$> cond_expr <*> arrow_type) <|> cond_expr

-- Handle to the top-level expression parser.
expr :: Parser Ast
expr = coerce_expr


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
base_type = (enclosed "(" arrow_type ")") <|> rec_type <|> literal_type <|> var

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