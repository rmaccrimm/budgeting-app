{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import Brick
    ( App
    , AttrMap
    , BrickEvent (VtyEvent)
    , Widget
    , attrMap
    , on
    , simpleMain
    , str
    , vBox
    , withBorderStyle
    , (<+>)
    )
import Brick.Focus                (focusGetCurrent, focusRingCursor)
import Brick.Forms
    ( Form (formState)
    , editTextField
    , formFocus
    , handleFormEvent
    , newForm
    , renderForm
    )
import Brick.Main
import Brick.Types                (EventM, Next)
import Brick.Widgets.Border       (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center       (center)
import Brick.Widgets.Core         (withAttr)
import Brick.Widgets.List         (GenericList (listSelected), handleListEvent, list, renderList)
import Control.Lens.Operators
import Control.Lens.TH            (makeLenses)
import Control.Monad              (void)
import Graphics.Vty               (Attr, black, cyan, white, yellow)

import qualified Data.Text    as T
import qualified Graphics.Vty as V

import Data.Zipper
import UI.Categories


attrs :: AttrMap
attrs = attrMap (white `on` black ) [("highlighted", black `on` cyan)]


handleEvent :: CategoryState e
  -> BrickEvent () e
  -> EventM () (Next (CategoryState e))
handleEvent st evt =
  if st ^. editing
  then
    case evt of
      (VtyEvent (V.EvKey V.KEsc []))   -> continue $ st & editing .~ False
                                                        & categories %~ deleteTree
      (VtyEvent (V.EvKey V.KEnter [])) -> continue st'
        where st' = st & editing .~ False
                       & categories . focus . label . name
                         .~ formState (st ^. editor) ^. txt
                       & editor .~ initCategoryForm
      _ -> do
        editor' <- handleFormEvent evt $ st ^. editor
        continue $ (editor .~ editor') st
  else
    case evt of
      (VtyEvent (V.EvKey (V.KChar 'q') [])) -> halt st
      (VtyEvent (V.EvKey V.KEsc []))        -> halt st
      (VtyEvent (V.EvKey V.KUp []))         -> continue $ st & categories %~ goLeft
      (VtyEvent (V.EvKey V.KDown  []))      -> continue $ st & categories %~ goRight
      (VtyEvent (V.EvKey V.KRight []))      -> continue $ st & categories %~ goDown
      (VtyEvent (V.EvKey V.KLeft []))       -> continue $ st & categories %~ goUp
      (VtyEvent (V.EvKey V.KDel []))        -> continue $ st & categories %~ deleteTree
      (VtyEvent (V.EvKey V.KEnter []))
        -> continue $ st & categories %~ addChild Category { _id = 1, _name = ""}
                         & editing .~ True
      _ -> continue st


app = App { appDraw = renderCategories
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attrs
          , appChooseCursor =  showFirstCursor
          }


main :: IO ()
main = void $ defaultMain app mkCategoryState 

