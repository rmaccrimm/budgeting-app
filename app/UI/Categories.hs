{-# LANGUAGE OverloadedStrings #-}
module UI.Categories where

import Data.Zipper
import Control.Lens.Operators

import Brick (App, AttrMap, BrickEvent (VtyEvent), Widget, attrMap, on, simpleMain, str, vBox,
              withBorderStyle, (<+>))
import Brick.Main
import Brick.Types (EventM, Next)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (withAttr)
import Brick.Widgets.List (GenericList (listSelected), handleListEvent, list, renderList)
import Graphics.Vty (Attr, black, cyan, white, yellow)
import qualified Graphics.Vty as V

spaces :: Int -> String
spaces n  = [' ' | _ <- [1..n]]


renderString :: Bool -> String -> Widget ()
renderString selected s = if selected
                          then withAttr "highlighted" $ str s
                          else str s


-- Only the root node of the tree is rendered since we want the tree to collapse as we walk back up
-- towards the root
renderTree :: Int -> Bool -> Tree String -> Widget ()
renderTree lvl selected t = renderString selected (spaces (2 * lvl) ++ symbol ++ t ^. label)
  where symbol = case t ^. children of
          [] -> "- "
          _  -> "+ "

-- Render a context in two parts - before and after the focused subtree
renderContext :: Int -> Maybe (Context String) -> (Widget (), Widget ())
renderContext lvl mCtx = case mCtx of
  Nothing -> (vBox [], vBox [])
  Just ctx -> (vBox $ before : paddedVal : rls, vBox $ rrs ++ [after])
    where paddedVal = str (spaces (2 * (lvl - 1)) ++ "o " ++ (ctx ^. pLabel))
          rls = map (renderTree lvl False) (reverse (ctx ^. lSiblings))
          rrs = map (renderTree lvl False) (ctx ^. rSiblings)
          (before, after) = renderContext (lvl - 1) (ctx ^. pContext)


getLevel :: Maybe (Context String) -> Int
getLevel Nothing = 0
getLevel (Just ctx) = 1 + getLevel (ctx ^. pContext)


renderLocation :: Location String -> [Widget ()]
renderLocation loc = case loc ^. context of
  Nothing  -> [renderTree 0 True (loc ^. focus)]
  Just ctx -> [vBox [before, renderTree lvl True (loc ^. focus), after]]
    where lvl = getLevel (loc ^. context)
          (before, after) = renderContext lvl (loc ^. context)
