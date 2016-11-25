module LambdaAST.Path
  (
    findNodeByPath,

    findFirstInnermostRedex,
    findFirstOutermostRedex,

    replaceByPath,

    Branch (Left, Right),
    Path
  )
where

import Prelude hiding (Left, Right)
import Data.Functor

import LambdaAST

data Branch = Left | Right
  deriving Show

newtype Path = Path [Branch]
  deriving Show

root = Path []

(<:>) :: Branch -> Path -> Path
(<:>) b (Path p) = Path (b:p)

goRightThen :: Path -> Path
goRightThen p = Right <:> p

goLeftThen :: Path -> Path
goLeftThen p = Left <:> p

findNodeByPath :: Path -> LambdaTerm -> Maybe LambdaTerm
findNodeByPath (Path []) term = Just term
findNodeByPath (Path (Right:ps)) Lambda { term = term } = findNodeByPath (Path ps) term
findNodeByPath (Path (Left:ps)) Application { function = left } = findNodeByPath (Path ps) left
findNodeByPath (Path (Right:ps)) Application { argument = right } = findNodeByPath (Path ps) right
findNodeByPath (Path (Right:ps)) Definition { value = right } = findNodeByPath (Path ps) right
findNodeByPath _ _ = Nothing

findNode :: LambdaTerm -> LambdaTerm -> [Path]
findNode n h = findNodeInSubTree [] n h
  where
    findNodeInSubTree :: [Branch] -> LambdaTerm -> LambdaTerm -> [Path]
    findNodeInSubTree p n h | n == h = [Path p]
    findNodeInSubTree p n Lambda { term = right} = findNodeInSubTree (p ++ [Right]) n right
    findNodeInSubTree p n Application { function = left, argument = right } =
      (findNodeInSubTree (p ++ [Left]) n left) ++ (findNodeInSubTree (p ++ [Right]) n right)
    findNodeInSubTree p n Variable {} = []
    findNodeInSubTree p n Definition { value = right } =
      findNodeInSubTree (p ++ [Right]) n right

findFirstInnermostRedex :: LambdaTerm -> Maybe Path
findFirstInnermostRedex Lambda { term = right } =
  goRightThen <$> findFirstInnermostRedex right
findFirstInnermostRedex a@Application { function = left, argument = right } =
  case goLeftThen <$> findFirstInnermostRedex left of
    p@Just {} -> p
    Nothing ->
      case goRightThen <$> findFirstInnermostRedex right of
        p@Just {} -> p
        Nothing -> if isRedex a then Just root else Nothing
findFirstInnermostRedex Definition { value = right } =
  goRightThen <$> findFirstInnermostRedex right
findFirstInnermostRedex _ = Nothing

findFirstOutermostRedex :: LambdaTerm -> Maybe Path
findFirstOutermostRedex Lambda { term = right } =
  goRightThen <$> findFirstInnermostRedex right
findFirstOutermostRedex a@Application { function = left, argument = right } =
  if isRedex a
  then Just root
  else
    case goLeftThen <$> findFirstOutermostRedex left of
      p@Just{} -> p
      Nothing -> goRightThen <$> findFirstOutermostRedex right
findFirstOutermostRedex Definition { value = right } =
  goRightThen <$> findFirstInnermostRedex right
findFirstOutermostRedex _ = Nothing

replaceByPath :: Path -> LambdaTerm -> LambdaTerm -> LambdaTerm
replaceByPath (Path []) t _ = t
replaceByPath (Path (Right:ps)) t l@Lambda { term = right } =
  l { term = replaceByPath (Path ps) t right }
replaceByPath (Path (Left:ps)) t a@Application { function = left } =
  a { function = replaceByPath (Path ps) t left }
replaceByPath (Path (Right:ps)) t a@Application { argument = right } =
  a { argument = replaceByPath (Path ps) t right }
replaceByPath (Path (Right:ps)) t d@Definition { value = right } =
  d { value = replaceByPath (Path ps) t right }
replaceByPath _ _ t = t
