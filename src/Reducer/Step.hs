module Reducer.Step
  (
    ReductionStep(..),
    Trace (..),

    reducible
  )
where

import LambdaAST
import LambdaAST.Path

data ReductionStep = Beta { path :: Path, newTerm :: LambdaTerm } |
                     Delta { path :: Path, newTerm :: LambdaTerm } |
                     NoReduction { newTerm :: LambdaTerm } |
                     Timeout
  deriving Show

data Trace = Trace { initialForm :: LambdaTerm,
                     steps :: [ReductionStep] }
  deriving Show

reducible :: ReductionStep -> Bool
reducible s@NoReduction {} = False
reducible _ = True
