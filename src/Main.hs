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
       else readEvalPrint context >>= repl
  where
    readEvalPrint context =
      do line <- getLine
         if isEmptyLine line
           then readEvalPrint context
           else case lambdaRead line of
                  Left e ->
                    (putStrLn $ show e) >> return context
                  Right term -> do
                    let (context', term') = lambdaEval context term in
                      (putStrLn $ show term') >> return context'

isEmptyLine :: String -> Bool
isEmptyLine [] = True
isEmptyLine _ = False
