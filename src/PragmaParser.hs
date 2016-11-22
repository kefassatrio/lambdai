module PragmaParser
  (
    parsePragma,

    Pragma
    (
      Pragma
    ),
    pragmaOption,
    pragmaValue
  )
where

import Text.ParserCombinators.Parsec

data Pragma = Pragma { pragmaOption :: String,
                       pragmaValue :: String}

instance Show Pragma where
  show Pragma { pragmaOption = option, pragmaValue = value } =
    "[" ++ option ++ " := " ++ value ++ "]"

key :: Parser String
key = many1 alphaNum

value = key

pragma = do
  char ':'
  k <- key
  spaces
  v <- value
  return (Pragma k v)

parsePragma :: String -> Either ParseError Pragma
parsePragma s = parse pragma "" s
