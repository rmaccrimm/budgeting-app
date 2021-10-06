module Zipper
where

data Tree a = Node a [Tree a]
  deriving (Show, Eq)

data Path a = Root
            | Path a [Tree a] (Path a) [Tree a]
  deriving (Show, Eq)

newtype Location a = Location (Tree a, Path a)
  deriving (Show, Eq)

-- Shift focus to first left sibling 
goLeft :: Location a -> Location a
goLeft loc@(Location (_, Root)) = loc
goLeft loc@(Location (_, Path _ [] _ _)) = loc
goLeft (Location (t, Path a (l:ls) parent rs)) = Location (l, Path a ls parent (t:rs))

-- Shift focus to first left sibling 
goRight :: Location a -> Location a
goRight loc@(Location (_, Root)) = loc
goRight loc@(Location (t, Path _ _ _ [])) = loc
goRight (Location (t, Path a ls parent (r:rs))) = Location (r, Path a (t:ls) parent rs)

goDown :: Location a -> Location a
goDown loc@(Location (Node _ [], _)) = loc
goDown (Location (Node a (c:cs), path)) = Location(c, Path a [] path cs)

goUp :: Location a -> Location a
goUp loc@(Location (_, Root)) = loc
goUp (Location (t, Path a ls parent rs)) = Location (Node a (reverse ls ++ (t:rs)), parent)

