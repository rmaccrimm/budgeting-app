{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import Category.IO
import Category.State
import Category.UI

import Brick                  (AttrMap, attrMap, on)
import Brick.Main
import Database.SQLite.Simple
import Graphics.Vty           (black, cyan, white)


attrs :: AttrMap
attrs = attrMap (white `on` black ) [("highlighted", black `on` cyan)]


app :: App (CategoryState e) e ()
app = App { appDraw = renderCategories
          , appHandleEvent = handleCategoryEvent
          , appStartEvent = return
          , appAttrMap = const attrs
          , appChooseCursor =  showFirstCursor
          }


main :: IO ()
main = do
  conn <- open "app.db"
  root <- loadRootLocation conn
  _ <- defaultMain app (mkCategoryState root)
  close conn
