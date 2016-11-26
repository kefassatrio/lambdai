module Reducer.Step
  (
    ReductionStep
    (
      Beta,
      Delta,
      NoReduction
    ),
    path,
    newTerm,
    reducible,

    Trace (Trace),
    initialForm,
    steps
  )
where

import LambdaAST
import LambdaAST.Path

data ReductionStep = Beta { path :: Path, newTerm :: LambdaTerm } |
                     Delta { path :: Path, newTerm :: LambdaTerm } |
                     NoReduction { newTerm :: LambdaTerm }
  deriving Show

data Trace = Trace { initialForm :: LambdaTerm,
                     steps :: [ReductionStep] }
  deriving Show

reducible :: ReductionStep -> Bool
reducible s@NoReduction {} = False
reducible _ = True
