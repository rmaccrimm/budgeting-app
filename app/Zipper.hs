module Zipper
where

data Tree a = Leaf a
            | Fork (Tree a) (Tree a)
  deriving (Show)

data Context a = Root
               | LeftChild (Tree a) (Context a)
               | RightChild (Tree a) (Context a)
  deriving (Show)

newtype Location a = Location (Tree a, Context a)
  deriving (Show)

left :: Location a -> Location a
left (Location (Fork l r, context)) = Location (l, LeftChild r context)
left leaf                           = leaf

right :: Location a -> Location a
right (Location (Fork l r, context)) = Location (r, RightChild l context)
right leaf                           = leaf

up :: Location a -> Location a
up (Location (t, Root))           = Location (t, Root)
up (Location (t, LeftChild r c))  = Location (Fork t r, c)
up (Location (t, RightChild l c)) = Location (Fork l t, c)
