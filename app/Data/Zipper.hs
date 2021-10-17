{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Zipper
where

import Lens.Micro
import Lens.Micro.TH


data Tree a = Node
  { _label    :: a
  , _children :: [Tree a]
  }
  deriving (Eq, Show)


data Context a = Context
  { _pLabel    :: a
  , _pContext  :: Maybe (Context a)
  , _lSiblings :: [Tree a]
  , _rSiblings :: [Tree a]
  }
  deriving (Eq, Show)


data Location a = Location
  { _focus   :: Tree a
  , _context :: Maybe (Context a)
  }
  deriving (Eq, Show)


makeLenses ''Tree
makeLenses ''Context
makeLenses ''Location


goLeft :: Location a -> Location a
goLeft loc = case loc ^. context of
  Nothing  -> loc
  Just ctx -> case ctx ^. lSiblings of
                []      -> loc
                (l:ls)  -> loc & focus .~ l
                               & context ?~ ctx'
                  where ctx' = ctx & lSiblings .~ ls
                                   & rSiblings .~ (loc ^. focus : ctx ^. rSiblings)


goRight :: Location a -> Location a
goRight loc = case loc ^. context of
  Nothing  -> loc
  Just ctx -> case ctx ^. rSiblings of
                []      -> loc
                (r:rs)  -> loc & focus .~ r
                               & context ?~ ctx'
                  where ctx' = ctx & rSiblings .~ rs
                                   & lSiblings .~ (loc ^. focus : ctx ^. lSiblings)


goDown :: Location a -> Location a
goDown loc = case loc ^. focus . children of
  []     -> loc
  (c:cs) -> loc & focus .~ c
                & context ?~ ctx'
    where ctx' = Context { _pLabel = loc ^. focus . label
                         , _pContext = loc ^. context
                         , _lSiblings = []
                         , _rSiblings = cs
                         }


goUp :: Location a -> Location a
goUp loc = case loc ^. context of
  Nothing  -> loc
  Just ctx -> loc & focus . label .~ ctx ^. pLabel
                  & focus . children .~ children'
                  & context .~ (ctx ^. pContext)
    where children' = ctx ^. lSiblings ++ (loc ^. focus : ctx ^. rSiblings)


addChild :: a -> Location a -> Location a
addChild newVal loc = loc & focus .~ Node {_label = newVal, _children = [] }
                          & context ?~ ctx'
  where ctx' = Context { _pLabel = loc ^. focus . label
                       , _pContext = loc ^. context
                       , _lSiblings = reverse (loc ^. focus . children)
                       , _rSiblings = []
                       }


deleteTree :: Location a -> Location a
deleteTree loc = case loc ^. context of
  Nothing  -> loc
  Just ctx -> case (ctx ^. lSiblings, ctx ^. rSiblings) of
    ([], [])   -> loc & focus .~ Node { _label = ctx ^. pLabel, _children = []}
                      & context .~ (ctx ^. pContext)
    (l:ls, []) -> loc' l ls []
    (_, r:rs)  -> loc' r [] rs
    where loc' = \f ls rs -> loc & focus .~ f
                                 & context ?~ ( ctx & lSiblings .~ ls
                                                    & rSiblings .~ rs )


getLevel :: Maybe (Context e) -> Int
getLevel Nothing    = 0
getLevel (Just ctx) = 1 + getLevel (ctx ^. pContext)
