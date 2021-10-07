module Data.Zipper
where

data Tree a = Node a [Tree a]
  deriving (Show, Eq)

data Context a = Root
            | Context a [Tree a] (Context a) [Tree a]
  deriving (Show, Eq)

newtype Location a = Location (Tree a, Context a)
  deriving (Show, Eq)

-- Shift focus to first left sibling 
goLeft :: Location a -> Location a
goLeft loc@(Location (_, Root)) = loc
goLeft loc@(Location (_, Context _ [] _ _)) = loc
goLeft (Location (t, Context a (l:ls) parent rs)) = Location (l, Context a ls parent (t:rs))

-- Shift focus to first left sibling 
goRight :: Location a -> Location a
goRight loc@(Location (_, Root)) = loc
goRight loc@(Location (t, Context _ _ _ [])) = loc
goRight (Location (t, Context a ls parent (r:rs))) = Location (r, Context a (t:ls) parent rs)

goDown :: Location a -> Location a
goDown loc@(Location (Node _ [], _)) = loc
goDown (Location (Node a (c:cs), path)) = Location(c, Context a [] path cs)

goUp :: Location a -> Location a
goUp loc@(Location (_, Root)) = loc
goUp (Location (t, Context a ls parent rs)) = Location (Node a (reverse ls ++ (t:rs)), parent)

addChild :: a -> Location a -> Location a
addChild newVal (Location (Node a children, p)) = Location (sub, Context a (reverse children) p [])
  where sub = Node newVal []

deleteTree :: Location a -> Location a
deleteTree loc@(Location (_, Root)) = loc
deleteTree (Location (t, Context a [] parent [])) = Location (Node a [], parent)
deleteTree (Location (t, Context a (l:ls) parent [])) = Location (l, Context a ls parent [])
deleteTree (Location (t, Context a ls parent (r:rs))) = Location(r, Context a ls parent rs)
