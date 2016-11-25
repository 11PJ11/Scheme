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

-- An atom is a letter or symbol, followed by any number of letters, digits,
-- or symbols:
parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol -- <|> is the Parsec choice combinator 
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

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
