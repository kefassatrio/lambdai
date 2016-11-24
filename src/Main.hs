import System.IO
import Parser
import Interpreter
import PragmaParser
import Control.Monad

main :: IO ()
main =
  do putStrLn $ "Welcome to λi!"
     hSetBuffering stdout NoBuffering
     repl defaultContext

repl :: Context -> IO ()
repl context =
  do putStr "λ> "
     eof <- isEOF
     if eof
       then putStrLn "Bye!"
       else getLine >>= evalLine context >>= repl

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

setPragma :: Pragma -> Context -> IO Context
setPragma Pragma { pragmaOption = "passBy", pragmaValue = "value" } c =
  return c { evalStrategy = byValue }
setPragma Pragma { pragmaOption = "passBy", pragmaValue = "name" } c =
  return c { evalStrategy = byName }
setPragma Pragma { pragmaOption = "evalBodies", pragmaValue = "t" } c =
  return c { evalStrategy = (evalStrategy c) { evalBodies = True }}
setPragma Pragma { pragmaOption = "evalBodies", pragmaValue = "f" } c =
  return c { evalStrategy = (evalStrategy c) { evalBodies = False }}
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
                            let (context', term') = lambdaEval context term in
                              (putStrLn $ show term') >> return context'
