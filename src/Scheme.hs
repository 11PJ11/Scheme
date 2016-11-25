module Scheme where
import           Text.ParserCombinators.Parsec hiding (spaces)

-- PARSING
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
             char '"'
             x <- many (noneOf "\"")
             char '"'
             return $ String x

-- RETURN VALUES
--Every constructor in an algebraic data type also acts like a function that
--turns its arguments into a value of its type. It also serves as a pattern that
--can be used in the left-hand side of a pattern-matching expression
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right _  -> "Found value"
