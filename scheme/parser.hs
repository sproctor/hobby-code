import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Error
import System.Environment
import Control.Monad
import Control.Monad.Error
import Numeric
import Data.Char
import Text.ParserCombinators.Parsec.Rfc2234
import Test.HUnit
import Data.Vector (fromList, Vector)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Character Char
    | Bool Bool
    | Float Float
    | Vector (Vector LispVal)
    deriving Eq

data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

escapedChar :: Parser Char
escapedChar = do
    char '\\'
    c <- anyChar 
    return $ case c of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> c

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedChar <|> (noneOf "\\\""))
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom $ first:rest

parseBool :: Parser LispVal
parseBool = (caseChar 't' >> return (Bool True)) <|> (caseChar 'f' >> return (Bool False))

parseOct :: Parser LispVal
parseOct = do
    caseChar 'o'
    liftM (Number . (readWith readOct)) $ many1 octDigit

parseHex :: Parser LispVal
parseHex = do
    caseChar 'x'
    liftM (Number . (readWith readHex)) $ many1 hexDigit

parseBinary :: Parser LispVal
parseBinary = do
    caseChar 'b'
    liftM (Number . (readWith readBin)) $ many1 (oneOf "01")

parseDRadix :: Parser LispVal
parseDRadix = do
    caseChar 'd'
    parseDecimal

parseDecimal :: Parser LispVal
parseDecimal
    =   parseFloat
    <|> liftM (Number . read) (many1 digit)

parseFloat1 :: Parser String
parseFloat1 = do
    c <- many1 digit
    char '.'
    rest <- many digit
    return $ c ++ ('.' : rest)

parseFloat2 :: Parser String
parseFloat2 = do
    char '.'
    rest <- many1 digit
    return $ "0." ++ rest

parseFloat :: Parser LispVal
parseFloat = try $ do
    f <- parseFloat1 <|> parseFloat2
    return $ (Float . (readWith readFloat)) f

parseRadix :: Parser LispVal
parseRadix
    =   parseOct
    <|> parseHex
    <|> parseBinary
    <|> parseDRadix

parseCharacter :: Parser LispVal
parseCharacter = do
    char '\\'
    liftM Character $ cspace <|> cnewline <|> anyChar
  where
    cspace = do
        try $ caseString "space"
        return ' '
    cnewline = do
        try $ caseString "newline"
        return '\n'

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasi :: Parser LispVal
parseQuasi = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do
    char '('
    exprs <- sepBy parseExpr spaces
    char ')'
    return $ Vector $ fromList exprs

parseHash :: Parser LispVal
parseHash = do
    char '#'
    parseBool <|> parseRadix <|> parseCharacter <|> parseVector

parseExpr :: Parser LispVal
parseExpr
    =   parseString
    <|> parseDecimal
    <|> parseHash
    <|> parseAtom
    <|> parseQuoted
    <|> parseQuasi
    <|> parseUnquote
    <|> do char '('
           x <- try parseList <|> parseDottedList
           char ')'
           return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readBin = readInt 2 (`elem` "01") digitToInt

readWith f s = fst $ f s !! 0

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values "
    ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
    maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
          ($ args)
          (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+", numericBinop (+))
    , ("-", numericBinop (-))
    , ("*", numericBinop (*))
    , ("/", numericBinop div)
    , ("mod", numericBinop mod)
    , ("quotient", numericBinop quot)
    , ("remainder", numericBinop rem)
    , ("symbol?", lispSymbol)
    , ("string?", lispString)
    , ("number?", lispNumber)
    , ("symbol->string", symbol2string)
    , ("string->symbol", string2symbol)
    ]

lispSymbol :: [LispVal] -> ThrowsError LispVal
lispSymbol [Atom _] = return $ Bool True
lispSymbol [_] = return $ Bool False
lispSymbol args = throwError $ NumArgs 1 args

lispString :: [LispVal] -> ThrowsError LispVal
lispString [String _] = Bool True
lispString [_] = Bool False
lispString args = throwError $ NumArgs 1 args

lispNumber :: [LispVal] -> ThrowsError LispVal
lispNumber [Number _] = return $ Bool True
lispNumber [Float _] = return $ Bool True
lispNumber [_] = return $ Bool False
lispNumber args = throwError $ NumArgs 1 args

symbol2string :: [LispVal] -> ThrowsError LispVal
symbol2string [Atom n] = return $ String n
symbol2string args = throwError NumArgs 1 args

string2symbol :: [LispVal] -> ThrowsError LispVal
string2symbol [String n] = return $ Atom n
string2symbol args = throwError NumArgs 1 args

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (Float n) = return $ truncate n
unpackNum (String n) =
    let parsed = reads n in
    if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

evalExpr :: String -> String
evalExpr = show . eval . readExpr

--main :: IO ()
main = do
    -- args <- getArgs
    -- putStrLn (readExpr (args !! 0))
    runTests

runTests = runTestTT $ TestList
  [ readExpr "413" ~?= Number 413
  , readExpr "#x10" ~?= Number 16
  , readExpr "#O20" ~?= Number 16
  , readExpr "#b101" ~?= Number 5
  , readExpr "#d2020" ~?= Number 2020

  , String "Hello, World!" ~=? readExpr "\"Hello, World!\""

  , Character '\n' ~=? readExpr "#\\newline"
  , Character 'A' ~=? readExpr "#\\A"
  , Character 'A' ~=? readExpr "#\\A "
  , Character '*' ~=? readExpr "#\\*B"
  , readExpr "#\\Space" ~=? Character ' '

  , Bool True ~=? readExpr "#t"
  , Bool False ~=? readExpr "#f"
  , readExpr "#T" ~?= Bool True

  , readExpr "mike" ~?= Atom "mike"
  , readExpr "\"\\\\mi\\nke\\\"\"" ~?= String "\\mi\nke\""

  , readExpr "3.14" ~?= Float 3.14
  , readExpr ".4" ~?= Float 0.4
  , readExpr "4." ~?= Float 4.0
  , readExpr "5.62500" ~?= Float 5.625
  , readExpr "\"this is a string\"" ~?= String "this is a string"
  , readExpr "25" ~?= Number 25
  , readExpr "symbol" ~?= Atom "symbol"
  , readExpr "(a test)" ~?= List [Atom "a", Atom "test"]
  , readExpr "(a (nested) test)" ~?= List [Atom "a", List [Atom "nested"], Atom "test"]
  , readExpr "(a (dotted . list) test)" ~?= (List [Atom "a", DottedList [Atom "dotted"] (Atom "list"), Atom "test"])
  , readExpr "(a '(quoted (dotted . list)) test)" ~?= (List [Atom "a", List [Atom "quote", List [Atom "quoted", DottedList [Atom "dotted"] (Atom "list")]], Atom "test"])
  , readExpr "`(1 2 3)" ~?= (List [Atom "quasiquote", List [Number 1, Number 2, Number 3]])
  , readExpr "`(list ,(+ 1 2) 4)" ~?= (List [Atom "quasiquote", List [Atom "list", List [Atom "unquote", List [Atom "+", Number 1, Number 2]], Number 4]])
  --, readExpr "(a '(imbalanced parens)" ~?= Left (ParseError "lisp")
  , show (List [Number 1, Number 2, Number 2]) ~?= "(1 2 2)"
  , show (List [Atom "quote", List [Number 1, Number 3, List [String "this", String "one"]]]) ~?= "(quote (1 3 (\"this\" \"one\")))"
  , evalExpr "'atom" ~?= "atom"
  , evalExpr "2" ~?= "2"
  , evalExpr "\"a string\"" ~?= "\"a string\""
  , evalExpr "(+ 2 2)" ~?= "4"
  , evalExpr "(+ 2 (-4 1))" ~?= "2"
  , evalExpr "(+ 2 (- 4 1))" ~?= "5"
  , evalExpr "(- (+ 4 6 3) 3 5 2)" ~?= "3"
  , evalExpr "(symbol? 'foo)" ~?= "#t"
  -- Need to implement car first, evalExpr "(symbol? (car '(a b)))" ~?= "#t"
  , evalExpr "(symbol? \"bar\")" ~?= "#f"
  , evalExpr "(symbol? '())" ~?= "#f"
  , evalExpr "(symbol? #f)" ~?= "#f"
  , evalExpr "(string? \"bar\")" ~?= "#t"
  , evalExpr "(string? '())" ~?= "#f"
  , evalExpr "(string? 'bar)" ~?= "#f"
  , evalExpr "(symbol->string 'foo)" ~?= "\"foo\""
  , evalExpr "(string->symbol \"foo\")" ~?= "foo"
  ]

