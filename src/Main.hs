import System.IO
import Control.Monad

import Parser
import PragmaParser
import Reducer
import Reducer.Renderer

main :: IO ()
main =
  do putStrLn $ "Welcome to λi!"
     hSetBuffering stdout NoBuffering
     repl defaultContext

repl :: Context -> IO ()
repl t =
  do putStr "λ> "
     eof <- isEOF
     if eof
       then putStrLn "Bye!"
       else getLine >>= evalLine t >>= repl

setPragma :: Pragma -> Context -> IO Context
setPragma Pragma { pragmaOption = "passBy", pragmaValue = value }
  c@Context { strategy = s } =
  case value of
    "name" -> return c { strategy = s { pass = ByName } }
    "value" -> return c { strategy = s { pass = ByValue} }
setPragma Pragma { pragmaOption = "evalOrder", pragmaValue = value }
  c@Context { strategy = s } =
  case value of
    "normal" -> return c { strategy = s { evalOrder = normalOrder } }
    "applicative" -> return c { strategy = s { evalOrder = applicativeOrder } }
setPragma Pragma { pragmaOption = "render", pragmaValue = value } c =
  case value of
    "cl" -> return c { renderer = clRendererSpec }
    "latex" -> return c { renderer = latexRendererSpec }
setPragma Pragma { pragmaOption = "load", pragmaValue = path } c =
  do contents <- readFile path
     foldM evalLine c $ lines contents
setPragma _ c = return c

evalLine :: Context -> String -> IO Context
evalLine context line | isEmptyLine line || isCommentLine line = return context
                      | isPragma line =
                        case parsePragma line of
                          Left e ->
                            (putStrLn $ show e) >> return context
                          Right pragma ->
                            setPragma pragma context
                      | otherwise =
                        case lambdaRead line of
                          Left e ->
                            (putStrLn $ show e) >> return context
                          Right term ->
                            let (table', trace) =
                                  reduceToNormalForm (strategy context) (table context) term in
                              (putStr $ render (renderer context) trace) >> return context { table = table' }

isEmptyLine :: String -> Bool
isEmptyLine [] = True
isEmptyLine _ = False

isCommentLine :: String -> Bool
isCommentLine [] = False
isCommentLine ('#':_) = True
isCommentLine _ = False

isPragma :: String -> Bool
isPragma [] = False
isPragma (':':_) = True
isPragma _ = False
