module Reducer.DefinitionTable
  (
    DefinitionTable (DefinitionTable),
    insert,
    delete,
    lookup,
    keySet,
    noDefs
  )
where

import Prelude hiding (lookup)

import LambdaAST

newtype DefinitionTable = DefinitionTable [(LambdaTerm, LambdaTerm)]

noDefs = DefinitionTable []

insert :: LambdaTerm -> LambdaTerm -> DefinitionTable -> DefinitionTable
insert k v (DefinitionTable t) = DefinitionTable ((k, v):t)

delete :: LambdaTerm -> DefinitionTable -> DefinitionTable
delete _ t@(DefinitionTable []) = t
delete k1 (DefinitionTable (d@(k2, _):ts))
  | k1 == k2 = DefinitionTable ts
  | otherwise = let DefinitionTable ts' = delete k1 $ DefinitionTable ts in
                  DefinitionTable $ d : ts'

lookup :: LambdaTerm -> DefinitionTable -> Maybe LambdaTerm
lookup _ (DefinitionTable []) = Nothing
lookup k1 (DefinitionTable ((k2,v):ts))
  | k1 == k2 = Just v
  | otherwise = lookup k1 (DefinitionTable ts)

keySet :: DefinitionTable -> TermSet
keySet (DefinitionTable t) = map fst t
