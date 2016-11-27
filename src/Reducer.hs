module Reducer
  (
    reduceToNormalForm,

    Strategy (Strategy),
    pass,
    evalOrder,

    EvaluationOrder (EvaluationOrder),
    normalOrder,
    applicativeOrder,

    Passing (ByName, ByValue),

    Context (Context),
    strategy,
    table,
    defaultContext,
    renderer,

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

data EvaluationOrder =
  EvaluationOrder { findRedex :: LambdaTerm -> Maybe Path,
                    findUnboundOcc :: TermSet -> LambdaTerm -> Maybe Path}
data Passing = ByName | ByValue

data Strategy = Strategy { pass :: Passing,
                           evalOrder :: EvaluationOrder }

data Context = Context { strategy :: Strategy,
                         table :: DefinitionTable,
                         renderer :: RendererSpec }

findNextRedex = findRedex . evalOrder

findNextUnboundOcc = findUnboundOcc . evalOrder

normalOrder = EvaluationOrder {
  findRedex = findFirstOutermostRedex,
  findUnboundOcc = findOutermostUnboundOccurrence }
applicativeOrder = EvaluationOrder {
  findRedex = findFirstInnermostRedex,
  findUnboundOcc = findInnermostUnboundOccurrence }

defaultStrategy = Strategy { pass = ByName, evalOrder = normalOrder }

defaultContext = Context { strategy = defaultStrategy,
                           table = noDefs,
                           renderer = clRendererSpec }

betaReduce :: Strategy -> LambdaTerm -> ReductionStep
betaReduce s@Strategy {} t =
  case findNextRedex s t of
    Nothing -> NoReduction t
    Just p ->
      let Just a = findNodeByPath p t
          step = (lambdaApply s p a) in
        Beta (path step) $ replaceByPath (path step) (newTerm step) t

lambdaApply :: Strategy -> Path -> LambdaTerm -> ReductionStep
lambdaApply Strategy { pass = ByName } p (Application (Lambda parameter term) argument) =
  Beta p $ replaceWithSubtree parameter argument term
lambdaApply s@Strategy { pass = ByValue } p
  (Application (Lambda parameter term) argument) =
  case findNextRedex s argument of
    Nothing -> Beta p $ replaceWithSubtree parameter argument term
    Just _ -> let step = betaReduce s argument in
                Beta (p <> path step) (newTerm step)
lambdaApply _ _ _ = undefined

deltaReduce :: Strategy -> DefinitionTable -> LambdaTerm -> ReductionStep
deltaReduce s d t = case findNextUnboundOcc s (keySet d) t of
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
