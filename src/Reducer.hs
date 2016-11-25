module Reducer
  (
  )
where

import LambdaAST
import LambdaAST.Path

data Context = DefinitionTable

type EvaluationOrder = LambdaTerm -> Maybe Path

data Passing = ByName | ByValue

data Strategy = Strategy { pass :: Passing,
                           evalOrder :: EvaluationOrder }

normalOrder = findFirstOutermostRedex
applicativeOrder = findFirstInnermostRedex

defaultStrategy = Strategy { pass = ByName, evalOrder = normalOrder }

betaReduce :: Strategy -> LambdaTerm -> LambdaTerm
betaReduce s@Strategy { evalOrder = evalOrder } t =
  case evalOrder t of
    Nothing -> t
    Just p ->
      case findNodeByPath p t of
        Nothing -> error "Error in code. (evalOrder or findNodeByPath)"
        Just a -> replaceByPath p (lambdaApply s a) t

lambdaApply :: Strategy -> LambdaTerm -> LambdaTerm
lambdaApply Strategy { pass = ByName } (Application (Lambda parameter term) argument) =
  replaceWithSubtree parameter argument term
lambdaApply s@Strategy { pass = ByValue, evalOrder = evalOrder }
  (Application (Lambda parameter term) argument) =
  case evalOrder argument of
    Nothing -> replaceWithSubtree parameter argument term
    Just _ -> betaReduce s term
lambdaApply _ _ = undefined
