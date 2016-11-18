import System.IO
import Parser
import Interpreter

main :: IO ()
main =
  do putStrLn $ "Welcome to λi!"
     hSetBuffering stdout NoBuffering
     repl byName

repl :: Context -> IO ()
repl context =
  do putStr "λ> "
     eof <- isEOF
     if eof
       then putStrLn "Bye!"
       else readEvalPrint context
  where
    readEvalPrint context =
      do line <- getLine
         if isEmptyLine line || isCommentLine line
           then putStrLn "" >> repl context
           else case lambdaRead line of
                  Left e ->
                    (putStrLn $ show e) >> return context >>= repl
                  Right term -> do
                    let (context', term') = lambdaEval context term in
                      (putStrLn $ show term') >> repl context'

isEmptyLine :: String -> Bool
isEmptyLine [] = True
isEmptyLine _ = False

isCommentLine :: String -> Bool
isCommentLine [] = False
isCommentLine ('#':_) = True
isCommentLine _ = False
