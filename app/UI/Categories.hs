{-# LANGUAGE OverloadedStrings #-}
module UI.Categories where

import Data.Zipper

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
renderTree lvl selected (Node x children) = renderString selected (spaces (2 * lvl) ++ symbol ++ x)
  where symbol = case children of
          [] -> "- "
          _  -> "+ "

-- Render a context in two parts - before and after the focused subtree
renderContext :: Int -> Context String -> (Widget (), Widget ())
renderContext _ Root = (vBox [], vBox [])
renderContext lvl (Context val ls p rs) = (vBox $ before : paddedVal : rls, vBox $ rrs ++ [after])
  where paddedVal = str (spaces (2 * (lvl - 1)) ++ "o " ++ val)
        rls = map (renderTree lvl False) (reverse ls)
        rrs = map (renderTree lvl False) rs
        (before, after) = renderContext (lvl - 1) p


getLevel :: Context String -> Int
getLevel Root           = 0
getLevel (Context _ _ p _) = 1 + getLevel p


renderLocation :: Location String -> [Widget ()]
renderLocation (Location (t, Root)) = [renderTree 0 True t]
renderLocation (Location (t, p))  = [vBox [before, renderTree lvl True t, after]]
  where lvl = getLevel p
        (before, after) = renderContext lvl p
