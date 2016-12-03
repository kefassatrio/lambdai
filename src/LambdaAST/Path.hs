{-# LANGUAGE LambdaCase #-}
module LambdaAST.Path
  (
    findNodeByPath,
    replaceByPath,

    TraversalOrder,
    outermostFirst,
    innermostFirst,
    outermostFirstNoLambda,
    innermostFirstNoLambda,

    findRedex,
    findFreeOccurrence,

    Branch (..),
    Path (..),
    descend,
    root
  )
where

import Prelude hiding (Left, Right, traverse)
import Data.Functor
import Data.Maybe

import LambdaAST

data Branch = Left | Right
  deriving (Show, Eq)

newtype Path = Path [Branch]
  deriving (Show, Eq)

root = Path []

instance Monoid Path where
  mempty = root
  mappend (Path a) (Path b) = Path $ a ++ b

(<:>) :: Branch -> Path -> Path
(<:>) b (Path p) = Path (b:p)

(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) a b = if isJust a then a else b

goRightThen :: Path -> Path
goRightThen p = Right <:> p

goLeftThen :: Path -> Path
goLeftThen p = Left <:> p

goRight :: Path -> Path
goRight (Path p) = Path $ p ++ [Right]

goLeft :: Path -> Path
goLeft (Path p) = Path $ p ++ [Left]

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

data Cursor = Cursor { path :: Path,
                       subTerm :: LambdaTerm,
                       bound :: TermSet } |
              CursorEnd
  deriving Eq

rootCursor :: LambdaTerm -> Cursor
rootCursor term = Cursor root term emptyTermSet

type PathSelector = (Cursor -> Cursor)

type TraversalOrder = [PathSelector]

right :: PathSelector
right = pathRight . termRight
  where
    pathRight = \case
      c@Cursor { path = path } -> c { path = goRight path }
      _ -> CursorEnd
    termRight = \case
      c@Cursor { subTerm = Lambda p right, bound = bound } ->
        c { subTerm = right, bound = addTerm (Variable p) bound }
      c@Cursor { subTerm = Application _ right } ->
        c { subTerm = right }
      c@Cursor { subTerm = Definition name right, bound = bound } ->
        c { subTerm = right, bound = addTerm (Variable name) bound }
      _ -> CursorEnd

left :: PathSelector
left = pathLeft . termLeft
  where
    pathLeft = \case
      c@Cursor { path = path } -> c { path = goLeft path }
      _ -> CursorEnd
    termLeft = \case
      c@Cursor { subTerm = Application left _ } -> c { subTerm = left }
      _ -> CursorEnd

rightNoLambda :: PathSelector
rightNoLambda = \case
  c@Cursor { subTerm = Lambda {} } -> CursorEnd
  c -> right c

self :: PathSelector
self = id

outermostFirst :: TraversalOrder
outermostFirst = [self, left, right]

innermostFirst :: TraversalOrder
innermostFirst = [left, right, self]

outermostFirstNoLambda :: TraversalOrder
outermostFirstNoLambda = [self, left, rightNoLambda]

innermostFirstNoLambda :: TraversalOrder
innermostFirstNoLambda = [left, rightNoLambda, self]

traverse :: TraversalOrder -> (Cursor -> Maybe a) -> LambdaTerm -> Maybe a
traverse selectors f t = traverse' selectors f $ rootCursor t
  where
    traverse' :: TraversalOrder -> (Cursor -> Maybe a) -> Cursor -> Maybe a
    traverse' _ _ CursorEnd = Nothing
    traverse' (s:[]) f c = let c' = s c in
      (f $ c') <|> traverseIfNotEqual f c c'
    traverse' (s:ss) f c = let c' = s c in
      (f $ c')
      <|> traverseIfNotEqual f c c'
      <|> traverse' ss f c
    traverseIfNotEqual f p c = (if p /= c then traverse' selectors f c else Nothing)

find :: TraversalOrder -> (Cursor -> Bool) -> LambdaTerm -> Maybe Path
find to p =
  traverse to (\c -> if p c then Just (path c) else Nothing)

findRedex :: TraversalOrder -> LambdaTerm -> Maybe Path
findRedex to = find to cursorIsRedex
  where
    cursorIsRedex c@Cursor { subTerm = term } = isRedex term
    cursorIsRedex CursorEnd = False

findFreeOccurrence :: TraversalOrder -> TermSet -> LambdaTerm -> Maybe Path
findFreeOccurrence to searchSet = find to occurrence
  where
    occurrence c@Cursor { subTerm = term, bound = bound } =
      not (term `elem` bound) && term `elem` searchSet
    occurrence CursorEnd = False

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
