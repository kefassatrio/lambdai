module Parser (lambdaRead)
where

import Data.Char
import Text.ParserCombinators.Parsec

import LambdaAST

definition :: Parser LambdaTerm
definition = do
  l <- variable
  spaces
  char '='
  spaces
  t <- lambdaTerm
  return $ Definition (var l) t

variable :: Parser LambdaTerm
variable =
  do var <- many1 $ satisfy (\c -> not (isSpace c) && not (elem c "\\λ.()="))
     return $ Variable var

application :: Parser LambdaTerm
application =
  chainl1 lambdaTermNoApp (spaces >> return Application)

lambdaAbstraction :: Parser LambdaTerm
lambdaAbstraction =
  do char 'λ' <|> char '\\'
     x <- variable
     char '.'
     spaces
     term <- lambdaTerm
     return $ Lambda (var x) term

topLevelLambdaTerm :: Parser LambdaTerm
topLevelLambdaTerm =   (try definition)
                   <|> lambdaTerm

lambdaTerm :: Parser LambdaTerm
lambdaTerm =   (try application)
           <|> lambdaTermNoApp

lambdaTermNoApp :: Parser LambdaTerm
lambdaTermNoApp =   (char '(' >> lambdaTerm >>= (\lt -> char ')' >> return lt))
                <|> lambdaAbstraction
                <|> variable

lambdaRead :: String -> Either ParseError LambdaTerm
lambdaRead s = parse (topLevelLambdaTerm >>= (\term -> eof >> return term)) "" (replaceNumerals(formatForMultiplication s ""))

formatForMultiplication :: [Char] -> [Char] -> [Char]
formatForMultiplication [] result = result
formatForMultiplication (s:xs) result = if isMultiplication s then (formatForMultiplication xs ([s] ++ result)) else (formatForMultiplication xs (result ++ [s]))

isMultiplication :: Char -> Bool
isMultiplication '*' = True
isMultiplication c = False

replaceNumerals :: String -> String
replaceNumerals (s:xs) = replaceNumeral (s) ++ replaceNumerals (xs)
replaceNumerals [] = []

replaceNumeral :: Char -> String
replaceNumeral '0' = "(\\s.(\\z.z))"
replaceNumeral '1' = "(\\s.(\\z.s(z)))"
replaceNumeral '2' = "(\\s.(\\z.s(s(z))))"
replaceNumeral '3' = "(\\s.(\\z.s(s(s(z)))))"
replaceNumeral '4' = "(\\s.(\\z.s(s(s(s(z))))))"
replaceNumeral '5' = "(\\s.(\\z.s(s(s(s(s(z)))))))"
replaceNumeral '6' = "(\\s.(\\z.s(s(s(s(s(s(z))))))))"
replaceNumeral '7' = "(\\s.(\\z.s(s(s(s(s(s(s(z)))))))))"
replaceNumeral '8' = "(\\s.(\\z.s(s(s(s(s(s(s(s(z))))))))))"
replaceNumeral '9' = "(\\s.(\\z.s(s(s(s(s(s(s(s(s(z)))))))))))"
replaceNumeral '+' = "(\\w.(\\y.(\\x.(y((w y)x)))))"
replaceNumeral '+' = "(\\w.(\\y.(\\x.(y((w y)x)))))"
replaceNumeral '*' = "(\\x.(\\y.(\\z.(x(y z)))))"
replaceNumeral c = [c]
