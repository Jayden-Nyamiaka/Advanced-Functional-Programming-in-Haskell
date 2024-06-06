----------------------------------------------------------------------
-- Simple XML parser.
----------------------------------------------------------------------

-- We call this module `Main` for testing purposes.
-- In a real-world scenario, it would be called `XML`.
module Main where

import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

type Tag = String

-- A character entity.
data Entity = LT_E | GT_E | AMP_E
   deriving (Show)

-- A single element of an XML document.
data Elem =
    TextE String     -- raw text
  | EntE Entity      -- entity
  | FormE Tag [Elem] -- tagged data
  deriving (Show)

----------------------------------------------------------------------
-- Simple parsers.
----------------------------------------------------------------------

-- Helper function for Parse Entity that 
-- parses the specific entity type.
parseEntityType :: Parser Entity
parseEntityType = 
  try (string "lt" >> return LT_E) 
  <|> try (string "gt" >> return GT_E)
  <|> (string "amp" >> return AMP_E)
  <?> "valid entity"

-- Parse an entity.
parseEntity :: Parser Entity
parseEntity = do
  char '&'
  ent <- parseEntityType
  char ';'
  return ent
  <?> "entity"

-- Parse some text not containing any tags or entities.
parseText :: Parser String
parseText = many1 (noneOf "<>&") <?> "text"

-- Parse a tag name, which is a nonempty sequence of letters,
-- all alphabetic characters or digits (no symbols), either
-- lower-case or upper-case.
parseTag :: Parser Tag
parseTag = many1 alphaNum <?> "tag"

----------------------------------------------------------------------
-- Form parsers.
----------------------------------------------------------------------

-- Parse a single tagged form.
parseTagged :: Parser (Tag, [Elem])
parseTagged = do
  char '<'
  tg <- parseTag
  char '>'
  contents <- many1 parseElem
  char '<'
  char '/'
  string tg
  char '>'
  return (tg, contents)
  <?> "tagged form"

-- Parse a single XML expression.
parseElem :: Parser Elem
parseElem = 
  (parseEntity >>= return . EntE)
  <|> (parseText >>= return . TextE)
  <|> try (parseTagged >>= \(tg, elems) -> 
    return (FormE tg elems))
  <?> "XML element"

-- Parse a series of elements from a string representing the entire contents of
-- a file.
parseElemsFromFile :: Parser [Elem]
parseElemsFromFile = do
  xs <- many parseElem
  eof
  return xs 
  <?> "file of XML elements"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print an XML element.
ppXML :: Int -> Elem -> String
ppXML i (TextE t)  = indent i ++ "Text[\n" ++ t ++ "\n" ++ indent i ++ "]\n"
ppXML i (EntE e)   = indent i ++ "Entity[" ++ show e ++ "]"
ppXML i (FormE t es) = 
  indent i ++ "Form: {" ++ t ++ "}[\n" 
  ++ concatMap (\s -> ppXML (i + 2) s ++ "\n") es
  ++ indent i ++ "]{/" ++ t ++ "}\n"

-- Parse all expressions in a file and run the pretty-printer on them.
runPpXML :: FilePath -> IO ()
runPpXML f = do
  p <- parseFromFile parseElemsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppXML 0 s)
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
    [filename] -> runPpXML filename >> exitSuccess
    _ -> usage progName >> exitFailure
