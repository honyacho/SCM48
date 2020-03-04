{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           System.Environment
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Control.Monad
import           Numeric
import           Lib
import           Control.Monad.Error
import           System.IO
import           Data.IORef

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
             | LSPrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | LSFunc {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispError = LENumArgs Integer [LispVal]
               | LETypeMismatch String LispVal
               | LEParser ParseError
               | LEBadSpecialForm String LispVal
               | LENotFunction String String
               | LEUnboundVar String String
               | LEDefault String

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

showError :: LispError -> String
showError (LEUnboundVar     message varname) = message ++ ": " ++ varname
showError (LEBadSpecialForm message form   ) = message ++ ": " ++ show form
showError (LENotFunction    message func   ) = message ++ ": " ++ show func
showError (LENumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (LETypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (LEParser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where
  show = showError
instance Error LispError where
  noMsg  = LEDefault "An error has occurred"
  strMsg = LEDefault
type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> throwError $ LEParser err
  Right val -> return val


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (LSString contents) = "\"" ++ contents ++ "\""
showVal (LSAtom   name    ) = name
showVal (LSNumber contents) = show contents
showVal (LSBool   True    ) = "#t"
showVal (LSBool   False   ) = "#f"
showVal (LSList   contents) = "(" ++ unwordsList contents ++ ")"
showVal (LSDottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (LSPrimitiveFunc _) = "<primitive>"
showVal (LSFunc { params = args, vararg = varargs, body = body, closure = env })
  = "(lambda ("
    ++ unwords (map show args)
    ++ (case varargs of
         Nothing  -> ""
         Just arg -> " . " ++ arg
       )
    ++ ") ...)"
showVal (Port   _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

escapeStr :: Parser String
escapeStr = do
  back <- char '\\'
  val  <- oneOf "nrt\"\\"
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
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> LSBool True
    "#f" -> LSBool False
    _    -> LSAtom atom

parseDecimal :: Parser LispVal
parseDecimal = do
  optional $ try $ string "#d"
  sign <- fmap (maybe 1 (const (-1))) $ optionMaybe $ char '-'
  digs <- many1 digit
  return $ LSNumber $ sign * (read digs)

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  sign <- fmap (maybe 1 (const (-1))) $ optionMaybe $ char '-'
  digs <- many1 $ oneOf "01234567"
  return $ LSNumber $ sign * (fst $ readOct digs !! 0)

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  sign <- fmap (maybe 1 (const (-1))) $ optionMaybe $ char '-'
  digs <- many1 $ oneOf "0123456789abcdef"
  return $ LSNumber $ sign * (fst $ readHex digs !! 0)

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = parseDecimal <|> parseOct <|> parseHex


parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseAtom <|> parseString <|> parseQuoted <|> do
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

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+"        , numericBinop (+))
  , ("-"        , numericBinop (-))
  , ("*"        , numericBinop (*))
  , ("/"        , numericBinop div)
  , ("="        , numBoolBinop (==))
  , ("<"        , numBoolBinop (<))
  , (">"        , numBoolBinop (>))
  , ("/="       , numBoolBinop (/=))
  , (">="       , numBoolBinop (>=))
  , ("<="       , numBoolBinop (<=))
  , ("&&"       , boolBoolBinop (&&))
  , ("||"       , boolBoolBinop (||))
  , ("string=?" , strBoolBinop (==))
  , ("string<?" , strBoolBinop (<))
  , ("string>?" , strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("mod"      , numericBinop mod)
  , ("quotient" , numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("car"      , car)
  , ("cdr"      , cdr)
  , ("cons"     , cons)
  , ("eq?"      , eqv)
  , ("eqv?"     , eqv)
  , ("equal?"   , equal)
  ]


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(LSString _                               ) = return val
eval env val@(LSNumber _                               ) = return val
eval env val@(LSBool   _                               ) = return val
eval env (    LSAtom   id                              ) = getVar env id
eval env (    LSList   [LSAtom "quote", val]           ) = return val
eval env (    LSList   [LSAtom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    LSBool False -> eval env alt
    otherwise    -> eval env conseq
eval env (LSList [LSAtom "set!", LSAtom var, form]) =
  eval env form >>= setVar env var
eval env (LSList [LSAtom "define", LSAtom var, form]) =
  eval env form >>= defineVar env var
eval env (LSList (LSAtom "define" : LSList (LSAtom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (LSList (LSAtom "define" : LSDottedList (LSAtom var : params) varargs : body))
  = makeVarargs varargs env params body >>= defineVar env var
eval env (LSList (LSAtom "lambda" : LSList params : body)) =
  makeNormalFunc env params body
eval env (LSList (LSAtom "lambda" : LSDottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (LSList (LSAtom "lambda" : varargs@(LSAtom _) : body)) =
  makeVarargs varargs env [] body
eval env (LSList (function : args)) = do
  func    <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm =
  throwError $ LEBadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (LSPrimitiveFunc func) args = liftThrows $ func args
apply (LSFunc params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ LENumArgs (num params) args
    else
      (liftIO $ bindVars closure $ zip params args)
      >>= bindVarArgs varargs
      >>= evalBody
 where
  remainingArgs = drop (length params) args
  num           = toInteger . length
  evalBody env = liftM last $ mapM (eval env) body
  bindVarArgs arg env = case arg of
    Just argName -> liftIO $ bindVars env [(argName, LSList $ remainingArgs)]
    Nothing      -> return env

primitiveBindings =
  nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, LSPrimitiveFunc func)

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
 where
  extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
  addBinding (var, value) = do
    ref <- newIORef value
    return (var, ref)

numericBinop
  :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ LENumArgs 2 singleVal
numericBinop op params =
  mapM unpackNum params >>= return . LSNumber . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LSNumber n) = return n
unpackNum (LSString n) =
  let parsed = reads n
  in  if null parsed
        then throwError $ LETypeMismatch "number" $ LSString n
        else return $ fst $ parsed !! 0
unpackNum (LSList [n]) = unpackNum n
unpackNum notNum       = throwError $ LETypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (LSString s) = return s
unpackStr (LSNumber s) = return $ show s
unpackStr (LSBool   s) = return $ show s
unpackStr notString    = throwError $ LETypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (LSBool b) = return b
unpackBool notBool    = throwError $ LETypeMismatch "boolean" notBool

boolBinop
  :: (LispVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
  then throwError $ LENumArgs 2 args
  else do
    left  <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return $ LSBool $ left `op` right
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [LSList (x : xs)        ] = return x
car [LSDottedList (x : xs) _] = return x
car [badArg                 ] = throwError $ LETypeMismatch "pair" badArg
car badArgList                = throwError $ LENumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [LSList (x : xs)        ] = return $ LSList xs
cdr [LSDottedList [xs    ] x] = return x
cdr [LSDottedList (_ : xs) x] = return $ LSDottedList xs x
cdr [badArg                 ] = throwError $ LETypeMismatch "pair" badArg
cdr badArgList                = throwError $ LENumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, LSList []            ] = return $ LSList [x1]
cons [x , LSList xs            ] = return $ LSList $ x : xs
cons [x , LSDottedList xs xlast] = return $ LSDottedList (x : xs) xlast
cons [x1, x2                   ] = return $ LSDottedList [x1] x2
cons badArgList                  = throwError $ LENumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(LSBool   arg1), (LSBool arg2)  ] = return $ LSBool $ arg1 == arg2
eqv [(LSNumber arg1), (LSNumber arg2)] = return $ LSBool $ arg1 == arg2
eqv [(LSString arg1), (LSString arg2)] = return $ LSBool $ arg1 == arg2
eqv [(LSAtom   arg1), (LSAtom arg2)  ] = return $ LSBool $ arg1 == arg2
eqv [(LSDottedList xs x), (LSDottedList ys y)] =
  eqv [LSList $ xs ++ [x], LSList $ ys ++ [y]]
eqv [(LSList arg1), (LSList arg2)] =
  return
    $  LSBool
    $  (length arg1 == length arg2)
    && (all eqvPair $ zip arg1 arg2)
 where
  eqvPair (x1, x2) = case eqv [x1, x2] of
    Left  err          -> False
    Right (LSBool val) -> val
eqv [_, _]     = return $ LSBool False
eqv badArgList = throwError $ LENumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
      unpacked1 <- unpacker arg1
      unpacked2 <- unpacker arg2
      return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM
    (unpackEquals arg1 arg2)
    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ LSBool $ (primitiveEquals || let (LSBool x) = eqvEquals in x)
equal badArgList = throwError $ LENumArgs 2 badArgList

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result then return () else action result >> until_ pred prompt action

makeFunc varargs env params body =
  return $ LSFunc (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl =
  primitiveBindings
    >>= until_ (== "quit") (readPrompt "Lisp>>> ")
    .   evalAndPrint

isBound :: Env -> String -> IO Bool
isBound envRef var =
  readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ LEUnboundVar "Getting an unbound variable: " var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ LEUnboundVar "Setting an unbound variable: " var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env      <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0         -> runRepl
    1         -> runOne $ args !! 0
    otherwise -> putStrLn "Program takes only 0 or 1 argument"
