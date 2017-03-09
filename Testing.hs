-- For testing purposes.

import Text.Parsec (parse)
import Parser (repl_parse, Ast)
import Typing (Expr, synth_expr, init_ctx, eval, debug_rep)



simple_parse :: String -> Either String Ast
simple_parse s = case parse repl_parse "simple_exec" s of
  Left x  -> Left $ show x
  Right x -> Right x

simple_exec :: String -> Either String String
simple_exec s = do
  ast    <- simple_parse s
  (e, t) <- debug_rep $ synth_expr init_ctx ast
  val    <- debug_rep $ eval e
  Right $ show val ++ ": " ++ show t