----------------------------------------------------------------------
-- S-expression parser.
----------------------------------------------------------------------

-- We call this module `Main` for testing purposes.
-- In a real-world scenario, it would be called `Sexpr`.
module Main where

import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA   Bool
  | IntA    Integer
  | FloatA  Double
  | IdA     String  -- identifier
  | StringA String  -- basic string
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"

parseExponentAfterFloat :: Parser String
parseExponentAfterFloat = do
  (char 'e' <|> char 'E')
  sign <- option "" (string "-" <|> string "+")
  digits <- many1 digit
  return $ 'e' : sign ++ digits
  <?> "integer power"

parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  trailExp <- option "" parseExponentAfterFloat
  return (read (sign ++ digits ++ "." ++ f ++ trailExp) :: Double)
  <?> "floating-point number"

parseString :: Parser String
parseString = do
  char '\"'
  s <- many1 (noneOf "\"")
  char '\"'
  return s
  <?> "string"

parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> try (parseString >>= return . StringA)
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"

-- Helper function for parseList which returns a list parser for a 
-- specific set of given opening and closing parentheses.
parseListWithDelim :: Char -> Char -> Parser [Sexpr]
parseListWithDelim open close = do
  char open
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  char close
  return ss

-- C.3 Why not try?
-- In our case, try combinators are not needed in the parseList function 
-- because backtracking is not necessary. In the event the parser fails on
-- one kind of delimiter, the parser won't consume that delimiter, and no
-- other parser wil be able to handle it either because no other parsers 
-- consume any brackets, braces, or parentheses. Thus, with no parsers able
-- to consume the delimiter, it will result in failure on its own. This 
-- failure is the desired behavior if two delimiters don't match such that
-- we don't need the try combinator's backtracking. The <|> operator's will
-- also trivially guarantee that all delimiters are checked for.

-- Parse a list of S-expressions, delimited by parentheses,
-- separated by whitespace/comments.
parseList :: Parser [Sexpr]
parseList = do
  parseListWithDelim '(' ')'
  <|> parseListWithDelim '[' ']'
  <|> parseListWithDelim '{' '}'
  <?> "list of S-expressions"

-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = do
  char '\''
  expr <- parseSexpr
  return $ ListS [AtomS (IdA "quote"), expr]
  <?> "quoted S-expression"

-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> parseQuote
  <?> "S-expression"

-- Parse a series of Sexprs from a string
-- representing the entire contents of a file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ "AtomS[" ++ show a ++ "]"
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"

-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

usage :: String -> IO ()
usage s = do
  hPutStrLn stderr $ "usage: " ++ s ++ " filename"

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [filename] -> runPpSexpr filename >> exitSuccess
    _ -> usage progName >> exitFailure
