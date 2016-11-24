import System.IO
import Parser
import Interpreter
import PragmaParser

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
       else getLine >>= parseInput
  where
    parseInput line | isEmptyLine line || isCommentLine line = repl context
    parseInput line | isPragma line =
                      case parsePragma line of
                        Left e ->
                          (putStrLn $ show e) >> repl context
                        Right pragma ->
                          repl $ setPragma pragma context
    parseInput line = case lambdaRead line of
                        Left e ->
                          (putStrLn $ show e) >> repl context
                        Right term ->
                          let (context', term') = lambdaEval context term in
                            (putStrLn $ show term') >> repl context'

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

setPragma :: Pragma -> Context -> Context
setPragma Pragma { pragmaOption = "passBy", pragmaValue = "value" } c =
  c { evalStrategy = byValue }
setPragma Pragma { pragmaOption = "passBy", pragmaValue = "name" } c =
  c { evalStrategy = byName }
setPragma Pragma { pragmaOption = "evalBodies", pragmaValue = "t" } c =
  c { evalStrategy = (evalStrategy c) { evalBodies = True }}
setPragma Pragma { pragmaOption = "evalBodies", pragmaValue = "f" } c =
  c { evalStrategy = (evalStrategy c) { evalBodies = False }}
setPragma _ c = c
