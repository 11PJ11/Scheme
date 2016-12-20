module Scheme where
import           Control.Monad
import           Text.ParserCombinators.Parsec hiding (spaces)

(|>) x f = f x

-- *****************************************************************************
-- RETURN VALUES
-- *****************************************************************************
--Every constructor in an algebraic data type also acts like a function that
--turns its arguments into a value of its type. It also serves as a pattern that
--can be used in the left-hand side of a pattern-matching expression
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- *****************************************************************************
-- EVALUATION
-- *****************************************************************************

-- The notation val@(String _) matches against any LispVal that's a string and
-- then binds val to the whole LispVal, and not just the contents of the String
-- constructor
eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val
eval (List (Atom funcName : args))  = map eval args
                                    |> apply funcName

apply :: String -> [LispVal] -> LispVal
apply funcName args =
  maybe (Bool False) ($ args) $ lookup funcName primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params =  map unpackNum params
                       |> foldl1 op
                       |> Number

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)] in
  if null parsed
    then 0
    else fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

-- *****************************************************************************
-- PRESENTATION
-- *****************************************************************************

instance Show LispVal
  where show = showVal
-- The unwordsList function works like the Haskell Prelude's unwords function,
-- which glues together a list of words with spaces.
-- Since we're dealing with a list of LispVals instead of words,
-- we define a function that first converts the LispVals into their
-- string representations and then applies unwords to it
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Pattern matching is a way of destructuring an algebraic data type, selecting a
-- code clause based on its constructor and then binding the components to variables.
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name)       = name
showVal (Number contents) = show contents
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- *****************************************************************************
-- PARSING
-- *****************************************************************************

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

parseNumber :: Parser LispVal
parseNumber =
  -- >>= notation
  many1 digit >>= \digits ->
    return $ (Number . read) digits
-- DOT NOTATION
-- liftM (Number . read) $ many1 digit
-- DO NOTATION
  --do
  --number <- many1 digit
  --return $ (Number . read) number

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                list <- try parseList <|> parseDottedList
                char ')'
                return list

-- Paranthesized lists
parseList :: Parser LispVal
parseList =
  parseExpr `sepBy` spaces >>= \expressions ->
    return $ List expressions

-- (a . (b . nil)) ---> [[a],[b]]
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- parseExpr `endBy` spaces -- "(a "
  tail <- char '.' >> spaces >> parseExpr -- ". (b . nil))"
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- EXPRESSION READING

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val
