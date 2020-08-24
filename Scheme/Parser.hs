{-|
Module      : Scheme.Parser
Description : Contains logic for the Scheme parser
License     : GPL-3
Maintainer  : jaredforthdev@gmail.com
Stability   : experimental
-}

module Scheme.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

import System.IO

import Data.IORef

import Control.Monad.Except

-- | Define a parser that recognizes allowed symbols in Scheme
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- | Define a function to call parser and handle errors
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

-- | Ignore spaces when parsing input
spaces :: Parser ()
spaces = skipMany1 space

-- | A datatype that can hold any Lisp value
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                     body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

-- | Function for string parsing
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

-- | Function for parsing an atom, which is is a letter or symbol,
-- followed by any number of letters, digits, or symbols.
parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- | Function for parsing a number
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- | Function for parsing a list
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- | Function for parsing a dotted list
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- | Function for parsing a single-quoted value
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- | Function for parsing an expression, which can be
-- am atom, string, number, quoted, list, or dotted list.
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

-- | Define a type for the environment
type Env = IORef [(String, IORef LispVal)]

-- | An empty environment
nullEnv :: IO Env
nullEnv = newIORef []

-- | Error handling functionality on top of IO monad
type IOThrowsError = ExceptT LispError IO

-- | Lift errors of upper type into monad
-- See: https://wiki.haskell.org/All_About_Monads#Lifting
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- | Catch errors
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

-- | Determine if a variable is already bound in the environment
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- | Retrieve current value of a variable
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

-- | Set variable value
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

-- | Define a new variable, regardless of if it is already bound
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

instance Show LispError where show = showError

-- | Define datatype to represent an error
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- | Make an instance of `Show`, and define how to print out error types
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

-- | Type to represent functions that may throw an error or return a value
type ThrowsError = Either LispError

-- | Helper function to convert error to string representation
trapError action = catchError action (return . show)

-- | Extract error value
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

-- | Helper function to create a function
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
-- | Specialization of `makeFunc` for a normal function
makeNormalFunc = makeFunc Nothing
-- | Specialization of `makeFunc` for a variable argument
makeVarArgs = makeFunc . Just . showVal

-- | Read and parse a file full of statements
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

-- | Define `apply` function
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
-- | Map `eval` function over the arguments
apply (PrimitiveFunc func) args = liftThrows $ func args

apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env

apply (IOFunc func) args = func args

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

-- | Create the evaluator. This maps a 'code' datatype to a 'data' datatype.
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
   load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- | Function to specify how to display possible `LispVals`
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

-- | Helper function to glue a list of words together with spaces
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- | Make `LispVal` a member of the class `Show`
instance Show LispVal where show = showVal