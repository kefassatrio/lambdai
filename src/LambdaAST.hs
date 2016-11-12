module LambdaAST
  (
    lambdaPrint,
    replaceWithSubtree,

    LambdaTerm
    (
      Lambda,
      Application,
      Variable,
      Definition
    ),
    parameter,
    term,
    function,
    argument,
    var,
    name,
    value
  )
where

import qualified Data.Set as Set

import Debug.Trace

data LambdaTerm = Lambda { parameter :: String,
                           term :: LambdaTerm} |
                  Application { function :: LambdaTerm,
                                argument :: LambdaTerm} |
                  Variable { var :: String } |
                  Definition { name :: String,
                               value :: LambdaTerm}
  deriving Eq

instance Show LambdaTerm where
  show (Lambda parameter term)  = "(λ" ++ parameter ++ "." ++ show term ++ ")"
  show (Application function argument) = "(" ++ show function ++ " " ++ show argument ++ ")"
  show (Variable var) = var
  show (Definition name value) = name ++ " = " ++ show value

listVariables :: LambdaTerm -> Set.Set String
listVariables (Lambda param term) = Set.insert param (listVariables term)
listVariables (Application function argument) =
  Set.union (listVariables function) (listVariables argument)
listVariables (Variable var) = Set.insert var Set.empty
listVariables (Definition name value) = (listVariables value)

listParameters :: LambdaTerm -> Set.Set String
listParameters (Lambda param term) = Set.insert param (listParameters term)
listParameters  (Application function argument) =
  Set.union (listParameters function) (listParameters argument)
listParameters (Variable var) = Set.empty
listParameters (Definition name value) = (listParameters value)

generateUniqueId :: String -> LambdaTerm -> String
generateUniqueId id ast = generateId id 0 (listVariables ast)
  where
    generateId :: String -> Int -> Set.Set String -> String
    generateId id n reserved =
      let id' = id ++ (show n) in
        if id' `Set.member` reserved
        then generateId id (succ n) reserved
        else id'

replaceIdsWithUniqueIds :: LambdaTerm -> LambdaTerm -> LambdaTerm
replaceIdsWithUniqueIds source destination =
  foldr (\var dst -> replaceVariable var (Variable $ generateUniqueId var source) dst)
  destination
  (Set.intersection (listVariables source) (listParameters destination))

replaceWithSubtree :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
replaceWithSubtree k v ast = replaceWithSubtree' k v (replaceIdsWithUniqueIds v ast)

replaceWithSubtree' :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
replaceWithSubtree' k v l@(Lambda parameter term) =
  if k == parameter then l else Lambda parameter (replaceWithSubtree' k v term)
replaceWithSubtree' k v a@(Application function argument) =
  Application (replaceWithSubtree' k v function) (replaceWithSubtree' k v argument)
replaceWithSubtree' k v t@(Variable var) = if k == var then v else t
replaceWithSubtree' k v d@(Definition name value) =
  Definition name (replaceWithSubtree' k v value)

replaceVariable :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
replaceVariable k v@(Variable var) l@(Lambda parameter term) =
  if k == parameter then Lambda var (replaceVariable k v term) else Lambda parameter (replaceVariable k v term)
replaceVariable k v a@(Application function argument) =
  Application (replaceVariable k v function) (replaceVariable k v argument)
replaceVariable k v t@(Variable var) = if k == var then v else t
replaceVariable k v d@(Definition name value) =
  Definition name (replaceVariable k v value)

lambdaPrint :: LambdaTerm -> IO ()
lambdaPrint l = putStrLn $ show l
