module LambdaAST.Path
  (
    findNodeByPath,

    findFirstInnermostRedex,
    findFirstOutermostRedex,

    findInnermostUnboundOccurrence,
    findOutermostUnboundOccurrence,

    replaceByPath,

    Branch (Left, Right),
    Path (Path),
    descend,
    root
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

instance Monoid Path where
  mempty = root
  mappend (Path a) (Path b) = Path $ a ++ b

(<:>) :: Branch -> Path -> Path
(<:>) b (Path p) = Path (b:p)

goRightThen :: Path -> Path
goRightThen p = Right <:> p

goLeftThen :: Path -> Path
goLeftThen p = Left <:> p

descend :: Maybe Path -> Branch -> Maybe Path
descend (Just (Path (Right:ps))) Right = Just $ Path ps
descend (Just (Path (Left:ps))) Left = Just $ Path ps
descend _ _ = Nothing

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
      p@Just {} -> p
      Nothing -> goRightThen <$> findFirstOutermostRedex right
findFirstOutermostRedex Definition { value = right } =
  goRightThen <$> findFirstInnermostRedex right
findFirstOutermostRedex _ = Nothing

findInnermostUnboundOccurrence :: TermSet -> LambdaTerm -> Maybe Path
findInnermostUnboundOccurrence = findInnermostUnboundOccurrence' []
  where
    findInnermostUnboundOccurrence' :: TermSet -> TermSet -> LambdaTerm -> Maybe Path
    findInnermostUnboundOccurrence' bound searchSet t@(Lambda p right) =
      compareIfNothing bound searchSet t $
      goRightThen <$> findInnermostUnboundOccurrence' (Variable p:bound) searchSet right
    findInnermostUnboundOccurrence' bound searchSet t@(Application left right) =
      compareIfNothing bound searchSet t $
      case goLeftThen <$> findInnermostUnboundOccurrence' bound searchSet left of
        p@Just {} -> p
        Nothing -> goRightThen <$> findInnermostUnboundOccurrence' bound searchSet right
    findInnermostUnboundOccurrence' bound searchSet t@(Definition boundVar right) =
      compareIfNothing bound searchSet t $
      goRightThen <$> findInnermostUnboundOccurrence' (Variable boundVar:bound) searchSet right
    findInnermostUnboundOccurrence' bound searchSet t =
      compareIfNothing bound searchSet t Nothing
    compareIfNothing :: TermSet -> TermSet -> LambdaTerm -> Maybe Path -> Maybe Path
    compareIfNothing bound searchSet term Nothing =
      if not (term `elem` bound) && (term `elem` searchSet) then Just root else Nothing
    compareIfNothing _ _ _ p = p

findOutermostUnboundOccurrence :: TermSet -> LambdaTerm -> Maybe Path
findOutermostUnboundOccurrence = findOutermostUnboundOccurrence' []
  where
    findOutermostUnboundOccurrence' :: TermSet -> TermSet -> LambdaTerm -> Maybe Path
    findOutermostUnboundOccurrence' bound searchSet term |
      not (term `elem` bound) && (term `elem` searchSet) = Just root
    findOutermostUnboundOccurrence' bound searchSet t@(Lambda p right) =
      goRightThen <$> findOutermostUnboundOccurrence' (Variable p:bound) searchSet right
    findOutermostUnboundOccurrence' bound searchSet t@(Application left right) =
      case goLeftThen <$> findOutermostUnboundOccurrence' bound searchSet left of
        p@Just {} -> p
        Nothing -> goRightThen <$> findOutermostUnboundOccurrence' bound searchSet right
    findOutermostUnboundOccurrence' bound searchSet t@(Definition boundVar right) =
      goRightThen <$> findOutermostUnboundOccurrence' (Variable boundVar:bound) searchSet right
    findOutermostUnboundOccurrence' _ _ _ = Nothing

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
