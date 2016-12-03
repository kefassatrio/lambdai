module Reducer
  (
    reduceToNormalForm,
    limitSteps,

    Strategy (..),

    normalOrder,
    applicativeOrder,
    callByName,
    callByValue,

    Passing (..),

    Context (..),
    defaultContext,

    noDefs
  )
where

import Prelude hiding (lookup)
import Data.Monoid

import LambdaAST
import LambdaAST.Path
import Reducer.DefinitionTable
import Reducer.Step
import Reducer.Renderer

data Passing = ByName | ByValue

type Strategy = TraversalOrder

data Context = Context { strategy :: Strategy,
                         table :: DefinitionTable,
                         renderer :: RendererSpec,
                         evalStepLimit :: Int }

normalOrder = outermostFirst
applicativeOrder = innermostFirst
callByName = outermostFirstNoLambda
callByValue = innermostFirstNoLambda


defaultStrategy = normalOrder

defaultContext = Context { strategy = defaultStrategy,
                           table = noDefs,
                           renderer = clRendererSpec,
                           evalStepLimit = 0 }

betaReduce :: Strategy -> LambdaTerm -> ReductionStep
betaReduce s t =
  case findRedex s t of
    Nothing -> NoReduction t
    Just p ->
      let Just a = findNodeByPath p t
          step = (lambdaApply p a) in
        Beta (path step) $ replaceByPath (path step) (newTerm step) t

lambdaApply :: Path -> LambdaTerm -> ReductionStep
lambdaApply p (Application (Lambda parameter term) argument) =
  Beta p $ replaceWithSubtree parameter argument term
lambdaApply _ _ = undefined

deltaReduce :: Strategy -> DefinitionTable -> LambdaTerm -> ReductionStep
deltaReduce s d t = case findFreeOccurrence s (keySet d) t of
                      Nothing -> NoReduction t
                      Just p ->
                        let Just key = (findNodeByPath p t)
                            Just replacement = (lookup key d) in
                          Delta p $ replaceByPath p replacement t

reduce :: Strategy -> DefinitionTable -> LambdaTerm -> ReductionStep
reduce s d t = case betaReduce s t of
                 b@Beta {} -> b
                 NoReduction {} -> deltaReduce s d t

reduceToNormalForm :: Strategy -> DefinitionTable -> LambdaTerm -> (DefinitionTable, Trace)
reduceToNormalForm s d t@(Definition name value) = (insert (Variable name) value d, Trace t [])
reduceToNormalForm s d t = (d, Trace t $ takeWhile reducible (trace t))
  where
    trace t =
      let step = reduce s d t in
        step:(trace $ newTerm step)

limitSteps :: Int -> Trace -> Trace
limitSteps 0 t = t
limitSteps n t = t { steps =
                       let s' = take n $ steps t
                           l = length $ s' in
                         if l < n
                         then s'
                         else s' ++ [Timeout] }
