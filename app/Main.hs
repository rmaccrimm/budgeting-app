{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Zipper

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


data Category = Category { name          :: String
                         , expanded      :: Bool
                         , subcategories :: [Category]
                         }
  deriving (Show)


testTree = Node "Categories"
            [ Node "bills"
              [ Node "phone" []
              , Node "rent" []
              ]
            , Node "toplevel" []
            , Node "expenses"
              [ Node "fun"
                [ Node "eating out" []
                , Node "games" []
                ]
              , Node "necessities" []
              ]
            ]

testLoc = Location (testTree, Root)


attrs :: AttrMap
attrs = attrMap (white `on` black ) [("highlighted", black `on` cyan)]


spaces :: Int -> String
spaces n  = [' ' | _ <- [1..n]]


renderString :: Bool -> String -> Widget ()
renderString selected s = if selected
                          then withAttr "highlighted" $ str s
                          else str s

-- Render a subtree with the given indentation level. Second arg indicates if root is highlighted
renderTree :: Int -> Bool -> Tree String -> Widget ()
renderTree lvl selected  (Node x children) = vBox $ currentLine : rest
  where currentLine = renderString selected (spaces (2 * lvl) ++ x)
        rest = case children of
          []    -> []
          trees -> map (renderTree (lvl + 1) False) trees


-- Render a context in two parts - before and after the focused subtree
renderPath :: Int -> Path String -> (Widget (), Widget ())
renderPath _ Root = (vBox [], vBox [])
renderPath lvl (Path val ls p rs) = (vBox $ before : paddedVal : rls, vBox $ rrs ++ [after])
  where paddedVal = str (spaces (2 * (lvl - 1)) ++ val)
        rls = map (renderTree lvl False) (reverse ls)
        rrs = map (renderTree lvl False) rs
        (before, after) = renderPath (lvl - 1) p


getLevel :: Path String -> Int
getLevel Root           = 0
getLevel (Path _ _ p _) = 1 + getLevel p


renderLocation :: Location String -> [Widget ()]
renderLocation (Location (t, Root)) = [renderTree 0 True t]
renderLocation (Location (t, p))  = [vBox [before, renderTree lvl True t, after]]
  where lvl = getLevel p
        (before, after) = renderPath lvl p




handleEvent :: Location String -> BrickEvent () e -> EventM () (Next (Location String))
handleEvent loc evt = case evt of
  (VtyEvent (V.EvKey (V.KChar 'q') [])) -> halt loc
  (VtyEvent (V.EvKey V.KEsc []))        -> halt loc
  (VtyEvent (V.EvKey V.KUp []))         -> continue $ goLeft loc
  (VtyEvent (V.EvKey V.KDown  []))      -> continue $ goRight loc
  (VtyEvent (V.EvKey V.KRight []))      -> continue $ goDown loc
  (VtyEvent (V.EvKey V.KLeft []))       -> continue $ goUp loc
  _                                     -> continue loc


app :: App (Location String) e ()
app =
    App { appDraw = renderLocation
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const attrs
        , appChooseCursor = neverShowCursor
        }


main :: IO ()
main =
  let initialSt = (goDown . goDown . goRight . goRight . goDown) testLoc
   in do
    fSt <- defaultMain app initialSt
    return ()


