{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Zipper
import UI.Categories

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


testTree = Node "Categories"
             [ Node "bills"
               [ Node "phone" []
               , Node "rent" []
               ]
             , Node "toplevel"
               [ Node "a" []
               ]
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


handleEvent :: Location String
  -> BrickEvent () e
  -> EventM () (Next (Location String))
handleEvent loc evt = case evt of
  (VtyEvent (V.EvKey (V.KChar 'q') [])) -> halt loc
  (VtyEvent (V.EvKey V.KEsc []))        -> halt loc
  (VtyEvent (V.EvKey V.KUp []))         -> continue $ goLeft loc
  (VtyEvent (V.EvKey V.KDown  []))      -> continue $ goRight loc
  (VtyEvent (V.EvKey V.KRight []))      -> continue $ goDown loc
  (VtyEvent (V.EvKey V.KLeft []))       -> continue $ goUp loc
  (VtyEvent (V.EvKey V.KEnter []))      -> continue $ addChild "new" loc
  (VtyEvent (V.EvKey V.KDel []))        -> continue $ deleteTree loc
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


