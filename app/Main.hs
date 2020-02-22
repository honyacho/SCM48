module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Lib
import Control.Monad.Error

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = LSAtom String
             | LSList [LispVal]
             | LSDottedList [LispVal] LispVal
             | LSNumber Integer
             | LSString String
             | LSBool Bool

data LispError = LENumArgs Integer [LispVal]
               | LETypeMismatch String LispVal
               | LEParser ParseError
               | LEBadSpecialForm String LispVal
               | LENotFunction String String
               | LEUnboundVar String String
               | LEDefault String

showError :: LispError -> String
showError (LEUnboundVar message varname) = message ++ ": " ++ varname
showError (LEBadSpecialForm message form) = message ++ ": " ++ show form
showError (LENotFunction message func) = message ++ ": " ++ show func
showError (LENumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (LETypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (LEParser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
instance Error LispError where
  noMsg = LEDefault "An error has occurred"
  strMsg = LEDefault
type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ LEParser err
  Right val -> return val


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (LSString contents) = "\"" ++ contents ++ "\""
showVal (LSAtom name) = name
showVal (LSNumber contents) = show contents
showVal (LSBool True) = "#t"
showVal (LSBool False) = "#f"
showVal (LSList contents) = "(" ++ unwordsList contents ++ ")"
showVal (LSDottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

escapeStr :: Parser String
escapeStr = do
  back <- char '\\'
  val <- oneOf "nrt\"\\"
  return $ back : val : []

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapeStr <|> many1 (noneOf "\\\"")
  char '"'
  return $ LSString $ foldl (++) "" x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> LSBool True
    "#f" -> LSBool False
    _    -> LSAtom atom

parseDecimal :: Parser LispVal
parseDecimal = do
  optional $ try $ string "#d"
  digs <- many1 digit
  return $ LSNumber $ read digs

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  digs <- many1 $ oneOf "01234567"
  return $ LSNumber $ fst $ readOct digs !! 0

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  digs <- many1 $ oneOf "0123456789abcdef"
  return $ LSNumber $ fst $ readHex digs !! 0

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = parseDecimal <|> parseOct <|> parseHex


parseExpr :: Parser LispVal
parseExpr = parseNumber
  <|> parseAtom
  <|> parseString
  <|> parseQuoted
  <|> do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

parseList :: Parser LispVal
parseList = liftM LSList $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ LSDottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ LSList [LSAtom "quote", x]

-- apply :: String -> [LispVal] -> LispVal
-- apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]


eval :: LispVal -> ThrowsError LispVal
eval val@(LSString _) = return val
eval val@(LSNumber _) = return val
eval val@(LSBool _) = return val
eval (LSList [LSAtom "quote", val]) = return val
eval (LSList (LSAtom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ LEBadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ LENotFunction "Unrecognized primitive function args" func)
        ($ args)
        (lookup func primitives)


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ LENumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . LSNumber . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LSNumber n) = return n
unpackNum (LSString n) = let parsed = reads n in
                          if null parsed
                            then throwError $ LETypeMismatch "number" $ LSString n
                            else return $ fst $ parsed !! 0
unpackNum (LSList [n]) = unpackNum n
unpackNum notNum = throwError $ LETypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (LSString s) = return s
unpackStr (LSNumber s) = return $ show s
unpackStr (LSBool s) = return $ show s
unpackStr notString = throwError $ LETypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (LSBool b) = return b
unpackBool notBool = throwError $ LETypeMismatch "boolean" notBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ LENumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ LSBool $ left `op` right
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [LSList (x : xs)] = return x
car [LSDottedList (x : xs) _] = return x
car [badArg] = throwError $ LETypeMismatch "pair" badArg
car badArgList = throwError $ LENumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [LSList (x : xs)] = return $ LSList xs
cdr [LSDottedList [xs] x] = return x
cdr [LSDottedList (_ : xs) x] = return $ LSDottedList xs x
cdr [badArg] = throwError $ LETypeMismatch "pair" badArg
cdr badArgList = throwError $ LENumArgs 1 badArgList

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
