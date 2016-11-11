import System.IO
import Data.Either
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec

import Debug.Trace

data LambdaTerm = Lambda { parameter :: String,
                           term :: LambdaTerm} |
                  Application { function :: LambdaTerm,
                                argument :: LambdaTerm} |
                  Variable { var :: String } |
                  Constant { name :: String} |
                  Definition { name :: String,
                               value :: LambdaTerm} |
                  Closure { capturedEnv :: Environment,
                            parameter :: String,
                            term :: LambdaTerm }
  deriving Eq

data Environment = Environment { frame :: Frame,
                                 parent :: Environment } |
                   Root
  deriving Eq

data Frame = Frame { assocs :: Map.Map String LambdaTerm}
  deriving Eq

instance Show LambdaTerm where
  show l@Lambda {} = "(λ" ++ parameter l ++ "." ++ show (term l) ++ ")"
  show a@Application {} = "(" ++ show (function a) ++ " " ++ show (argument a) ++ ")"
  show v@Variable {} = var v
  show c@Constant {} = name c
  show d@Definition {} = (name d) ++ " = " ++ show (value d)
  show c@Closure {} = "(λ" ++ parameter c ++ "." ++ show (term c) ++ ")"

showBinding (k, v) = k ++ " := " ++ show v

instance Show Frame where
  show (Frame f) =
    let (h:t) = Map.assocs f in
      "[" ++ foldr (\a s -> s ++ ", " ++ showBinding a) (showBinding h) t ++ "]"

instance Show Environment where
  show Root = "{}"
  show e@Environment {} =
    "{" ++ show (frame e) ++ ", " ++ show (parent e) ++ "}"

empty = Frame Map.empty

frameBindings = assocs . frame

descend :: Environment -> Environment
descend env = Environment empty env

ascend :: Environment -> Environment
ascend Root = Root           -- don't ascend when already on top
ascend env = parent env

bindInFrame :: Frame -> String -> LambdaTerm -> Frame
bindInFrame f@Frame {} k v = Frame $ Map.insert k v (assocs f)

bind :: Environment -> String -> LambdaTerm -> Environment
bind Root k v = bind (descend Root) k v
bind env k v = Environment (bindInFrame (frame env) k v) (parent env)

varLookup :: Environment -> String -> LambdaTerm
varLookup Root k = Constant k
varLookup env k = ascendOrYield $ Map.lookup k (frameBindings env)
  where
    ascendOrYield :: Maybe LambdaTerm -> LambdaTerm
    ascendOrYield Nothing = varLookup (ascend env) k
    ascendOrYield (Just v) = v

variable :: Parser LambdaTerm
variable =
  do var <- many1 $ noneOf "λ \t\n\r.()"
     return $ Variable var

application :: Parser LambdaTerm
application =
  chainl1 lambdaTermNoApp (spaces >> return Application)

lambdaAbstraction :: Parser LambdaTerm
lambdaAbstraction =
  do char 'λ'
     x <- variable
     char '.'
     term <- lambdaTerm
     return $ Lambda (var x) term

assignment :: Parser LambdaTerm
assignment = do
  l <- variable
  spaces
  char '='
  spaces
  t <- lambdaTerm
  return $ Definition (var l) t

topLevelLambdaTerm :: Parser LambdaTerm
topLevelLambdaTerm =   (try assignment)
                   <|> lambdaTerm

lambdaTerm :: Parser LambdaTerm
lambdaTerm =   (try application)
           <|> lambdaTermNoApp

lambdaTermNoApp :: Parser LambdaTerm
lambdaTermNoApp =   (char '(' >> lambdaTerm >>= (\lt -> char ')' >> return lt))
                <|> lambdaAbstraction
                <|> variable

lRead s = parse topLevelLambdaTerm "" s

lEval :: Environment -> LambdaTerm -> (Environment, LambdaTerm)
-- lEval env x | trace ("eval  " ++ (show env) ++ " ; " ++ (show x)) False = undefined
lEval env (Variable v) = lEval env $ varLookup env v
lEval env c@Constant {} = (env, c)
lEval env (Application f a) = lApply env f a
lEval env l@Lambda {} = (env, Closure env (parameter l) (term l))
lEval env c@Closure {} = (env, c)
lEval env d@Definition {} = (bind env (name d) (value d), (value d))

lApply :: Environment -> LambdaTerm -> LambdaTerm -> (Environment, LambdaTerm)
-- lApply env f a | trace ("apply " ++ (show env) ++ " ; " ++ (show f) ++ " ; " ++ (show a)) False = undefined
lApply env f@Constant {} a = (env, Application f (snd $ lEval env a))
lApply env f@Closure {} a =
  let (_, a') = lEval env a
      env' = descend (capturedEnv f)
      env'' = bind env' (parameter f) a'
      (_, result) = lEval env'' (term f) in
    (env, result)
lApply env f a =
  let (env', f') = lEval env f in
    if f == f'
    then (env, Application f evalAndDropEnv)
    else lApply env' f' a
  where
   evalAndDropEnv =
     let (_, a') = lEval env a in
       a'

main =
  do putStrLn $ "Welcome to λi!"
     hSetBuffering stdout NoBuffering
     repl $ Root

repl :: Environment -> IO ()
repl env =
  do putStr "λ> "
     eofp <- isEOF
     if eofp then putStrLn "Bye!"
       else readEvalPrint env >>= (\newEnv -> repl newEnv)
       where
         readEvalPrint env = do
           str <- getLine
           env' <- lPrint $ lRead str
           return env'
         lPrint (Left e) = (putStrLn $ show e) >> return env
         lPrint (Right r) =
           let (env', result) = lEval env r in
             (putStrLn $ show $ result) >> return env'
